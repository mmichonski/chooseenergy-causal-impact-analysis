



        # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
        ###     Causal Impact Analysis for Plan Changes         ### 
        # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #




        # Determine whether testing locally or launching to production -- 'dev' = local and 'prod' = production 
        launch.container <- "prod"
        
        # Load required packages #
        library(httr)
        library(plyr)
        library(dplyr)
        library(R.utils)
        library(RMixpanel)
        library(RColorBrewer)
        library(RGoogleAnalytics)
        library(shiny)
        library(scales)
        library(ggplot2)
        library(lubridate)
        library(reshape2)
        library(RMySQL)
        library(ggthemes)
        library(shinydashboard)
        library(tidyr)
        library(xlsx)
        library(magrittr)
        library(gplots)
        library(DT)
        library(data.table)
        library(plotly)
        library(ggplot2)
        library(grid)
        library(gtable)
        library(stringr)
        library(shinyjs)
        library(DBI)
        library(CausalImpact)
        
        
        ## Define Needed Functions ##
        percent2 <- function(x){
            sprintf("%1.2f%%", 100*x)
        }
        
        
        weekinterval <- function(x) { ## Only takes POSIX.ct ##
            if(wday(x) < 2){
                start = as.Date(x)
                end = as.Date(x)+6
            } else {
                start = as.Date(x) - wday(x) + 1
                end = as.Date(x) + (7 - wday(x))
            }
            data.frame(start, end)
        }
        
        weekinterval.bind <- function(x) { ## Only takes POSIX.ct ##
            if(wday(x) < 2){
                start = as.Date(x)
                end = as.Date(x)+6
            } else {
                start = as.Date(x) - wday(x) + 1
                end = as.Date(x) + (7 - wday(x))
            }
            paste(start, end, sep = "_")
        }
        
        
        
        firstDayMonth=function(x)
        {           
            first=as.Date(paste(year(x), "-", month(x), "-1", sep = ""))
            first
        }
        
        
        monthstart <- function(x) { ## Only takes POSIX.ct ##
            w.interval <- weekinterval(x)
            if(day(w.interval$start) > day(w.interval$end)){
                cusp <- w.interval
                if(wday(w.interval$end - day(w.interval$end) + 1) > 4){
                    cusp.end <- cusp
                    if(day(x) > 7){
                        m.start <- firstDayMonth(x)
                        cusp <- weekinterval(m.start)
                        if(wday(cusp$end - day(cusp$end) + 1) > 4){
                            cusp.end <- cusp
                            cusp.begin <- weekinterval(m.start + 7)
                        } else {
                            cusp.begin <- cusp
                        }
                    } else{
                        m.start <- firstDayMonth(x-7)
                        cusp <- weekinterval(m.start)
                        if(wday(cusp$end - day(cusp$end) + 1) > 4){
                            cusp.end <- cusp
                            cusp.begin <- weekinterval(m.start + 7)
                        } else {
                            cusp.begin <- cusp
                        }
                    }
                } else {
                    cusp.begin <- cusp
                }
                
            } else {
                m.start <- firstDayMonth(x)
                cusp <- weekinterval(m.start)
                if(wday(cusp$end - day(cusp$end) + 1) > 4){
                    cusp.end <- cusp
                    cusp.begin <- weekinterval(m.start + 7)
                } else {
                    cusp.begin <- cusp
                }
            }
            cusp.begin$start
        }
        
        
        days.in.year <- function(x){
            if(leap_year(year(x))){
                days.in.year = 366
            } else{
                days.in.year = 365
            }
            days.in.year
        }
        
        
        
        try.Causal.Impact <- function(time_series, pre_date, post_date, conf_int) {
            
            
            out <- tryCatch(
                {
                    CausalImpact(time_series, pre_date, post_date, alpha = conf_int)
                    # Just to highlight: if you want to use more than one 
                    # R expression in the "try" part then you'll have to 
                    # use curly brackets.
                    # 'tryCatch()' will return the last evaluated expression 
                    # in case the "try" part was completed successfully
                    
                    # message("This is the 'try' part")
                    
                    
                    # The return value of `readLines()` is the actual value 
                    # that will be returned in case there is no condition 
                    # (e.g. warning or error). 
                    # You don't need to state the return value via `return()` as code 
                    # in the "try" part is not wrapped insided a function (unlike that
                    # for the condition handlers for warnings and error below)
                    
                },
                
                error=function(cond) {
                    # message("Here's the original error message:")
                    # message(cond)
                    # Choose a return value in case of error
                    return(0)
                },
                warning=function(cond) {
                    # message("Here's the original warning message:")
                    # message(cond)
                    # Choose a return value in case of warning
                    return(0)
                },
                finally={
                    # NOTE:
                    # Here goes everything that should be executed at the end,
                    # regardless of success or error.
                    # If you want more than one expression to be executed, then you 
                    # need to wrap them in curly brackets ({...}); otherwise you could
                    # just have written 'finally=<expression>' 
                    # message("tryCatch executed")
                }
            )
            
            return(out)
            
            
        }
        
        
        
        
        # Multiple plot function
        #
        # ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
        # - cols:   Number of columns in layout
        # - layout: A matrix specifying the layout. If present, 'cols' is ignored.
        #
        # If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
        # then plot 1 will go in the upper left, 2 will go in the upper right, and
        # 3 will go all the way across the bottom.
        #
        multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
            library(grid)
            
            # Make a list from the ... arguments and plotlist
            plots <- c(list(...), plotlist)
            
            numPlots = length(plots)
            
            # If layout is NULL, then use 'cols' to determine layout
            if (is.null(layout)) {
                # Make the panel
                # ncol: Number of columns of plots
                # nrow: Number of rows needed, calculated from # of cols
                layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                                 ncol = cols, nrow = ceiling(numPlots/cols))
            }
            
            if (numPlots==1) {
                print(plots[[1]])
                
            } else {
                # Set up the page
                grid.newpage()
                pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
                
                # Make each plot, in the correct location
                for (i in 1:numPlots) {
                    # Get the i,j matrix positions of the regions that contain this subplot
                    matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
                    
                    print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                                    layout.pos.col = matchidx$col))
                }
            }
        }
        
        
        

        
        

        
        
        shinyServer(function(input, output, session) {
        
            
            
            
            
                            #### - #### - #### - #### - #### - #### - #### - ####
                            #                                                   #
                            #                                                   #
                            #                                                   #
                            #      Start Bayesian Causal Impact Analysis        #
                            #                                                   #
                            #                                                   #
                            #                                                   #
                            #### - #### - #### - #### - #### - #### - #### - ####
            
            
            
            
            
            # # # --- # # # --- # # # --- # # # --- # # # ---
            
            #                      TAB 1                    #  
            
            # # # --- # # # --- # # # --- # # # --- # # # ---
            
            
            
            
            
            
            
            output$likelihood.description.1 <- renderText({
                
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                leads <- get.causal.impact.data()[[7]]
                
                
                k <- 1
                
                change.date.i <- top.impacts.leads[k, "graph.date"]
                
                leads %>%
                    mutate(n = 1) %>%
                    unite(original_plan_plus_utility_id, original_plan_id, utility_id, sep = "-") %>%
                    aggregate(n~original_plan_plus_utility_id + date_submitted, data=., sum) -> leads.by.plan_plus_utility_id     
                
                lbp.spread <- spread(leads.by.plan_plus_utility_id,key = original_plan_plus_utility_id, value = n, fill = 0)
                
                lbp.zoo <- zoo(lbp.spread[,-1], lbp.spread[,1])
                
                merge(lbp.zoo, zoo(1, seq(today() - daysback, today(), by = 1)), all = TRUE) %>% 
                    .[,-(ncol(.))] %>%
                    fortify.zoo(.) %>%
                    reshape2::melt("Index") %>%
                    dplyr::rename(date_submitted = Index, original_plan_plus_utility_id = variable, n = value) %>%
                    separate(original_plan_plus_utility_id, into = c("original_plan_id", "utility_id"), sep = "-") -> lbp 
                
                
                til.i <- top.impacts.leads[k,]
                ticr.i <- top.impacts.cr[k,]
                
                if(between(til.i$Average_Relative_Effect, ticr.i$Avg_Rel_Eff_Lo_90_Conf_Interval, ticr.i$Avg_Rel_Eff_Up_90_Conf_Interval)){
                    
                    
                    leads.ct[leads.ct$utility_name_id == top.impacts.leads[k, "utility_name_id"],] %>%
                        filter(product == top.impacts.leads[k, "product"]) %>%
                        .[,!names(.) %in% c("utility_name_id", "product")] -> leads.i
                    
                    lbp %>%
                        filter(utility_id == top.impacts.leads[k, "utility_id"]) %>%
                        .[,!names(.) %in% c("utility_id")] %>%
                        spread(key = original_plan_id, value = n, fill = 0) -> lbp.i
                    
                    for.cor.i <- merge(leads.i, lbp.i, by = "date_submitted")
                    
                    arrange(reshape2::melt(cor(for.cor.i$n, for.cor.i[,-1:-2])),desc(value)) %>%
                        .[,!names(.) %in% c("Var1")] %>%
                        dplyr::rename(original_plan_id = Var2, cor = value) -> cor.i
                    
                    lbp.i %>%
                        .[,names(.) %in% c("date_submitted", cor.i$original_plan_id[1:5])] %>%
                        reshape2::melt("date_submitted") %>%
                        dplyr::rename(original_plan_id = variable, n = value) -> top.lbp
                    
                    changes %>%
                        filter(utility_name_id == top.impacts.leads[k, "utility_name_id"]) %>%
                        filter(original_plan_id == as.numeric(cor.i$original_plan_id[1])) -> changes.i
                    
                    changes.i <- arrange(changes.i[!duplicated(changes.i),], plan_audit_id)
                    
                    if(dim(changes.i)[1] == 0){
                        
                        description <- paste0("Both lead volume and conversion rate were down, although no particular plan changes occurred that could be traced back to. See Lead Volume of Top Correlating Plans below for possibile causes.")
                        
                    } else {
                        if(between(changes.i[1,"date"], change.date.i - 3, change.date.i + 3)){
                            
                            description <- paste0("This plan change is highly likely to have caused the observed change in lead and conversion rate.")
                            
                        } else {
                            description <- paste0("Both lead volume and conversion rate were down, although no particular plan changes occurred that could be traced back to. See Lead Volume of Top Correlating Plans below for possibile causes.")
                        }
                    }
                    
                } else{
                    description <- paste0("No plan changes were found relating to this impact, and conversion rate either stayed relatively the same or went up while leads went down. Therefore the impact can be traced back to a change in traffic in this utility zone.")
                }
                
                description
                
            })
            
            
            
            
            
            output$plan_name1.tab <- renderText({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                leads <- get.causal.impact.data()[[7]]
                
                
                k <- 1
                
                change.date.i <- top.impacts.leads[k, "graph.date"]
                
                leads %>%
                    mutate(n = 1) %>%
                    unite(original_plan_plus_utility_id, original_plan_id, utility_id, sep = "-") %>%
                    aggregate(n~original_plan_plus_utility_id + date_submitted, data=., sum) -> leads.by.plan_plus_utility_id     
                
                lbp.spread <- spread(leads.by.plan_plus_utility_id,key = original_plan_plus_utility_id, value = n, fill = 0)
                
                lbp.zoo <- zoo(lbp.spread[,-1], lbp.spread[,1])
                
                merge(lbp.zoo, zoo(1, seq(today() - daysback, today(), by = 1)), all = TRUE) %>% 
                    .[,-(ncol(.))] %>%
                    fortify.zoo(.) %>%
                    reshape2::melt("Index") %>%
                    dplyr::rename(date_submitted = Index, original_plan_plus_utility_id = variable, n = value) %>%
                    separate(original_plan_plus_utility_id, into = c("original_plan_id", "utility_id"), sep = "-") -> lbp 
                
                
                
                
                
                til.i <- top.impacts.leads[k,]
                ticr.i <- top.impacts.cr[k,]
                
                if(between(til.i$Average_Relative_Effect, ticr.i$Avg_Rel_Eff_Lo_90_Conf_Interval, ticr.i$Avg_Rel_Eff_Up_90_Conf_Interval)){
                    
                    
                    leads.ct[leads.ct$utility_name_id == top.impacts.leads[k, "utility_name_id"],] %>%
                        filter(product == top.impacts.leads[k, "product"]) %>%
                        .[,!names(.) %in% c("utility_name_id", "product")] -> leads.i
                    
                    lbp %>%
                        filter(utility_id == top.impacts.leads[k, "utility_id"]) %>%
                        .[,!names(.) %in% c("utility_id")] %>%
                        spread(key = original_plan_id, value = n, fill = 0) -> lbp.i
                    
                    for.cor.i <- merge(leads.i, lbp.i, by = "date_submitted")
                    
                    arrange(reshape2::melt(cor(for.cor.i$n, for.cor.i[,-1:-2])),desc(value)) %>%
                        .[,!names(.) %in% c("Var1")] %>%
                        dplyr::rename(original_plan_id = Var2, cor = value) -> cor.i
                    
                    lbp.i %>%
                        .[,names(.) %in% c("date_submitted", cor.i$original_plan_id[1:5])] %>%
                        reshape2::melt("date_submitted") %>%
                        dplyr::rename(original_plan_id = variable, n = value) -> top.lbp
                    
                    changes %>%
                        filter(utility_name_id == top.impacts.leads[k, "utility_name_id"]) %>%
                        filter(original_plan_id == as.numeric(cor.i$original_plan_id[1])) -> changes.i
                    
                    changes.i <- arrange(changes.i[!duplicated(changes.i),], plan_audit_id)
                    
                    if(dim(changes.i)[1] == 0){
                        
                        paste0("Multiple Possibilities")
                        
                    } else {
                        if(between(changes.i[1,"date"], change.date.i - 3, change.date.i + 3)){
                            
                            as.character(changes.i[1, c("plan_name")])
                            
                        } else {
                            paste0("Multiple Possibilities")
                        }
                        
                    }
                    
                } else{
                    paste0("Traffic Change")
                }
                
                
                
            })
            
            
            output$plan_term1.tab <- renderText({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                leads <- get.causal.impact.data()[[7]]
                
                
                k <- 1
                
                change.date.i <- top.impacts.leads[k, "graph.date"]
                
                leads %>%
                    mutate(n = 1) %>%
                    unite(original_plan_plus_utility_id, original_plan_id, utility_id, sep = "-") %>%
                    aggregate(n~original_plan_plus_utility_id + date_submitted, data=., sum) -> leads.by.plan_plus_utility_id     
                
                lbp.spread <- spread(leads.by.plan_plus_utility_id,key = original_plan_plus_utility_id, value = n, fill = 0)
                
                lbp.zoo <- zoo(lbp.spread[,-1], lbp.spread[,1])
                
                merge(lbp.zoo, zoo(1, seq(today() - daysback, today(), by = 1)), all = TRUE) %>% 
                    .[,-(ncol(.))] %>%
                    fortify.zoo(.) %>%
                    reshape2::melt("Index") %>%
                    dplyr::rename(date_submitted = Index, original_plan_plus_utility_id = variable, n = value) %>%
                    separate(original_plan_plus_utility_id, into = c("original_plan_id", "utility_id"), sep = "-") -> lbp 
                
                
                
                til.i <- top.impacts.leads[k,]
                ticr.i <- top.impacts.cr[k,]
                
                if(between(til.i$Average_Relative_Effect, ticr.i$Avg_Rel_Eff_Lo_90_Conf_Interval, ticr.i$Avg_Rel_Eff_Up_90_Conf_Interval)){
                    
                    
                    leads.ct[leads.ct$utility_name_id == top.impacts.leads[k, "utility_name_id"],] %>%
                        filter(product == top.impacts.leads[k, "product"]) %>%
                        .[,!names(.) %in% c("utility_name_id", "product")] -> leads.i
                    
                    lbp %>%
                        filter(utility_id == top.impacts.leads[k, "utility_id"]) %>%
                        .[,!names(.) %in% c("utility_id")] %>%
                        spread(key = original_plan_id, value = n, fill = 0) -> lbp.i
                    
                    for.cor.i <- merge(leads.i, lbp.i, by = "date_submitted")
                    
                    arrange(reshape2::melt(cor(for.cor.i$n, for.cor.i[,-1:-2])),desc(value)) %>%
                        .[,!names(.) %in% c("Var1")] %>%
                        dplyr::rename(original_plan_id = Var2, cor = value) -> cor.i
                    
                    lbp.i %>%
                        .[,names(.) %in% c("date_submitted", cor.i$original_plan_id[1:5])] %>%
                        reshape2::melt("date_submitted") %>%
                        dplyr::rename(original_plan_id = variable, n = value) -> top.lbp
                    
                    changes %>%
                        filter(utility_name_id == top.impacts.leads[k, "utility_name_id"]) %>%
                        filter(original_plan_id == as.numeric(cor.i$original_plan_id[1])) -> changes.i
                    
                    changes.i <- arrange(changes.i[!duplicated(changes.i),], plan_audit_id)
                    
                    if(dim(changes.i)[1] == 0){
                        
                        paste0(" ")
                        
                    } else {
                        if(between(changes.i[1,"date"], change.date.i - 3, change.date.i + 3)){
                            
                            as.character(paste(changes.i[1, c("contract_term")], " Months ", changes.i[1, c("contract_type")], sep = ""))
                            
                        } else {
                            paste0(" ")
                        }
                        
                    }
                    
                } else{
                    paste0(" ")
                }
                
            }) 
            
            
            output$plan_supplier1.tab <- renderText({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                leads <- get.causal.impact.data()[[7]]
                
                
                k <- 1
                
                change.date.i <- top.impacts.leads[k, "graph.date"]
                
                leads %>%
                    mutate(n = 1) %>%
                    unite(original_plan_plus_utility_id, original_plan_id, utility_id, sep = "-") %>%
                    aggregate(n~original_plan_plus_utility_id + date_submitted, data=., sum) -> leads.by.plan_plus_utility_id     
                
                lbp.spread <- spread(leads.by.plan_plus_utility_id,key = original_plan_plus_utility_id, value = n, fill = 0)
                
                lbp.zoo <- zoo(lbp.spread[,-1], lbp.spread[,1])
                
                merge(lbp.zoo, zoo(1, seq(today() - daysback, today(), by = 1)), all = TRUE) %>% 
                    .[,-(ncol(.))] %>%
                    fortify.zoo(.) %>%
                    reshape2::melt("Index") %>%
                    dplyr::rename(date_submitted = Index, original_plan_plus_utility_id = variable, n = value) %>%
                    separate(original_plan_plus_utility_id, into = c("original_plan_id", "utility_id"), sep = "-") -> lbp 
                
                
                
                til.i <- top.impacts.leads[k,]
                ticr.i <- top.impacts.cr[k,]
                
                if(between(til.i$Average_Relative_Effect, ticr.i$Avg_Rel_Eff_Lo_90_Conf_Interval, ticr.i$Avg_Rel_Eff_Up_90_Conf_Interval)){
                    
                    
                    leads.ct[leads.ct$utility_name_id == top.impacts.leads[k, "utility_name_id"],] %>%
                        filter(product == top.impacts.leads[k, "product"]) %>%
                        .[,!names(.) %in% c("utility_name_id", "product")] -> leads.i
                    
                    lbp %>%
                        filter(utility_id == top.impacts.leads[k, "utility_id"]) %>%
                        .[,!names(.) %in% c("utility_id")] %>%
                        spread(key = original_plan_id, value = n, fill = 0) -> lbp.i
                    
                    for.cor.i <- merge(leads.i, lbp.i, by = "date_submitted")
                    
                    arrange(reshape2::melt(cor(for.cor.i$n, for.cor.i[,-1:-2])),desc(value)) %>%
                        .[,!names(.) %in% c("Var1")] %>%
                        dplyr::rename(original_plan_id = Var2, cor = value) -> cor.i
                    
                    lbp.i %>%
                        .[,names(.) %in% c("date_submitted", cor.i$original_plan_id[1:5])] %>%
                        reshape2::melt("date_submitted") %>%
                        dplyr::rename(original_plan_id = variable, n = value) -> top.lbp
                    
                    changes %>%
                        filter(utility_name_id == top.impacts.leads[k, "utility_name_id"]) %>%
                        filter(original_plan_id == as.numeric(cor.i$original_plan_id[1])) -> changes.i
                    
                    changes.i <- arrange(changes.i[!duplicated(changes.i),], plan_audit_id)
                    
                    if(dim(changes.i)[1] == 0){
                        
                        paste0(" ")
                        
                    } else {
                        if(between(changes.i[1,"date"], change.date.i - 3, change.date.i + 3)){
                            
                            as.character(changes.i[1, c("supplier")])
                            
                        } else {
                            paste0(" ")
                        }
                        
                    }
                    
                } else{
                    paste0(" ")
                }
                
            })
            
            
            output$plan_active1.tab <- renderText({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                leads <- get.causal.impact.data()[[7]]
                
                
                k <- 1
                
                change.date.i <- top.impacts.leads[k, "graph.date"]
                
                leads %>%
                    mutate(n = 1) %>%
                    unite(original_plan_plus_utility_id, original_plan_id, utility_id, sep = "-") %>%
                    aggregate(n~original_plan_plus_utility_id + date_submitted, data=., sum) -> leads.by.plan_plus_utility_id     
                
                lbp.spread <- spread(leads.by.plan_plus_utility_id,key = original_plan_plus_utility_id, value = n, fill = 0)
                
                lbp.zoo <- zoo(lbp.spread[,-1], lbp.spread[,1])
                
                merge(lbp.zoo, zoo(1, seq(today() - daysback, today(), by = 1)), all = TRUE) %>% 
                    .[,-(ncol(.))] %>%
                    fortify.zoo(.) %>%
                    reshape2::melt("Index") %>%
                    dplyr::rename(date_submitted = Index, original_plan_plus_utility_id = variable, n = value) %>%
                    separate(original_plan_plus_utility_id, into = c("original_plan_id", "utility_id"), sep = "-") -> lbp 
                
                
                
                til.i <- top.impacts.leads[k,]
                ticr.i <- top.impacts.cr[k,]
                
                if(between(til.i$Average_Relative_Effect, ticr.i$Avg_Rel_Eff_Lo_90_Conf_Interval, ticr.i$Avg_Rel_Eff_Up_90_Conf_Interval)){
                    
                    
                    leads.ct[leads.ct$utility_name_id == top.impacts.leads[k, "utility_name_id"],] %>%
                        filter(product == top.impacts.leads[k, "product"]) %>%
                        .[,!names(.) %in% c("utility_name_id", "product")] -> leads.i
                    
                    lbp %>%
                        filter(utility_id == top.impacts.leads[k, "utility_id"]) %>%
                        .[,!names(.) %in% c("utility_id")] %>%
                        spread(key = original_plan_id, value = n, fill = 0) -> lbp.i
                    
                    for.cor.i <- merge(leads.i, lbp.i, by = "date_submitted")
                    
                    arrange(reshape2::melt(cor(for.cor.i$n, for.cor.i[,-1:-2])),desc(value)) %>%
                        .[,!names(.) %in% c("Var1")] %>%
                        dplyr::rename(original_plan_id = Var2, cor = value) -> cor.i
                    
                    lbp.i %>%
                        .[,names(.) %in% c("date_submitted", cor.i$original_plan_id[1:5])] %>%
                        reshape2::melt("date_submitted") %>%
                        dplyr::rename(original_plan_id = variable, n = value) -> top.lbp
                    
                    changes %>%
                        filter(utility_name_id == top.impacts.leads[k, "utility_name_id"]) %>%
                        filter(original_plan_id == as.numeric(cor.i$original_plan_id[1])) -> changes.i
                    
                    changes.i <- arrange(changes.i[!duplicated(changes.i),], plan_audit_id)
                    
                    if(dim(changes.i)[1] == 0){
                        
                        paste0(" ")
                        
                    } else {
                        if(between(changes.i[1,"date"], change.date.i - 3, change.date.i + 3)){
                            
                            new_plan_status = changes.i[changes.i$plan_audit_id == max(changes.i$plan_audit_id), "plan_status_name"]
                            
                            new_plan_status
                            
                        } else {
                            paste0(" ")
                        }
                        
                    }
                    
                } else{
                    paste0(" ")
                }
                
            })
            
            output$plan_id1.tab <- renderText({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                leads <- get.causal.impact.data()[[7]]
                
                
                k <- 1
                
                change.date.i <- top.impacts.leads[k, "graph.date"]
                
                leads %>%
                    mutate(n = 1) %>%
                    unite(original_plan_plus_utility_id, original_plan_id, utility_id, sep = "-") %>%
                    aggregate(n~original_plan_plus_utility_id + date_submitted, data=., sum) -> leads.by.plan_plus_utility_id     
                
                lbp.spread <- spread(leads.by.plan_plus_utility_id,key = original_plan_plus_utility_id, value = n, fill = 0)
                
                lbp.zoo <- zoo(lbp.spread[,-1], lbp.spread[,1])
                
                merge(lbp.zoo, zoo(1, seq(today() - daysback, today(), by = 1)), all = TRUE) %>% 
                    .[,-(ncol(.))] %>%
                    fortify.zoo(.) %>%
                    reshape2::melt("Index") %>%
                    dplyr::rename(date_submitted = Index, original_plan_plus_utility_id = variable, n = value) %>%
                    separate(original_plan_plus_utility_id, into = c("original_plan_id", "utility_id"), sep = "-") -> lbp 
                
                
                
                til.i <- top.impacts.leads[k,]
                ticr.i <- top.impacts.cr[k,]
                
                if(between(til.i$Average_Relative_Effect, ticr.i$Avg_Rel_Eff_Lo_90_Conf_Interval, ticr.i$Avg_Rel_Eff_Up_90_Conf_Interval)){
                    
                    
                    leads.ct[leads.ct$utility_name_id == top.impacts.leads[k, "utility_name_id"],] %>%
                        filter(product == top.impacts.leads[k, "product"]) %>%
                        .[,!names(.) %in% c("utility_name_id", "product")] -> leads.i
                    
                    lbp %>%
                        filter(utility_id == top.impacts.leads[k, "utility_id"]) %>%
                        .[,!names(.) %in% c("utility_id")] %>%
                        spread(key = original_plan_id, value = n, fill = 0) -> lbp.i
                    
                    for.cor.i <- merge(leads.i, lbp.i, by = "date_submitted")
                    
                    arrange(reshape2::melt(cor(for.cor.i$n, for.cor.i[,-1:-2])),desc(value)) %>%
                        .[,!names(.) %in% c("Var1")] %>%
                        dplyr::rename(original_plan_id = Var2, cor = value) -> cor.i
                    
                    lbp.i %>%
                        .[,names(.) %in% c("date_submitted", cor.i$original_plan_id[1:5])] %>%
                        reshape2::melt("date_submitted") %>%
                        dplyr::rename(original_plan_id = variable, n = value) -> top.lbp
                    
                    changes %>%
                        filter(utility_name_id == top.impacts.leads[k, "utility_name_id"]) %>%
                        filter(original_plan_id == as.numeric(cor.i$original_plan_id[1])) -> changes.i
                    
                    changes.i <- arrange(changes.i[!duplicated(changes.i),], plan_audit_id)
                    
                    
                    if(dim(changes.i)[1] == 0){
                        
                        paste0(" ")
                        
                    } else {
                        if(between(changes.i[1,"date"], change.date.i - 3, change.date.i + 3)){
                            
                            as.character(paste("Plan ID: ", changes.i[1, c("original_plan_id")], sep = ""))
                            
                        } else {
                            paste0(" ")
                        }
                        
                    }
                    
                } else{
                    paste0(" ")
                }
                
                
            })
            
            
            
            output$price_change1 <- renderText({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                leads <- get.causal.impact.data()[[7]]
                
                
                k <- 1
                
                change.date.i <- top.impacts.leads[k, "graph.date"]
                
                leads %>%
                    mutate(n = 1) %>%
                    unite(original_plan_plus_utility_id, original_plan_id, utility_id, sep = "-") %>%
                    aggregate(n~original_plan_plus_utility_id + date_submitted, data=., sum) -> leads.by.plan_plus_utility_id     
                
                lbp.spread <- spread(leads.by.plan_plus_utility_id,key = original_plan_plus_utility_id, value = n, fill = 0)
                
                lbp.zoo <- zoo(lbp.spread[,-1], lbp.spread[,1])
                
                merge(lbp.zoo, zoo(1, seq(today() - daysback, today(), by = 1)), all = TRUE) %>% 
                    .[,-(ncol(.))] %>%
                    fortify.zoo(.) %>%
                    reshape2::melt("Index") %>%
                    dplyr::rename(date_submitted = Index, original_plan_plus_utility_id = variable, n = value) %>%
                    separate(original_plan_plus_utility_id, into = c("original_plan_id", "utility_id"), sep = "-") -> lbp 
                
                
                
                til.i <- top.impacts.leads[k,]
                ticr.i <- top.impacts.cr[k,]
                
                if(between(til.i$Average_Relative_Effect, ticr.i$Avg_Rel_Eff_Lo_90_Conf_Interval, ticr.i$Avg_Rel_Eff_Up_90_Conf_Interval)){
                    
                    
                    leads.ct[leads.ct$utility_name_id == top.impacts.leads[k, "utility_name_id"],] %>%
                        filter(product == top.impacts.leads[k, "product"]) %>%
                        .[,!names(.) %in% c("utility_name_id", "product")] -> leads.i
                    
                    lbp %>%
                        filter(utility_id == top.impacts.leads[k, "utility_id"]) %>%
                        .[,!names(.) %in% c("utility_id")] %>%
                        spread(key = original_plan_id, value = n, fill = 0) -> lbp.i
                    
                    for.cor.i <- merge(leads.i, lbp.i, by = "date_submitted")
                    
                    arrange(reshape2::melt(cor(for.cor.i$n, for.cor.i[,-1:-2])),desc(value)) %>%
                        .[,!names(.) %in% c("Var1")] %>%
                        dplyr::rename(original_plan_id = Var2, cor = value) -> cor.i
                    
                    lbp.i %>%
                        .[,names(.) %in% c("date_submitted", cor.i$original_plan_id[1:5])] %>%
                        reshape2::melt("date_submitted") %>%
                        dplyr::rename(original_plan_id = variable, n = value) -> top.lbp
                    
                    changes %>%
                        filter(utility_name_id == top.impacts.leads[k, "utility_name_id"]) %>%
                        filter(original_plan_id == as.numeric(cor.i$original_plan_id[1])) -> changes.i
                    
                    changes.i <- arrange(changes.i[!duplicated(changes.i),], plan_audit_id)
                    
                    
                    
                    if(dim(changes.i)[1] == 0){
                        
                        paste0(" ")
                        
                    } else {
                        if(between(changes.i[1,"date"], change.date.i - 3, change.date.i + 3)){
                            
                            old_price = changes.i[changes.i$plan_audit_id == min(changes.i$plan_audit_id), "plan_price"]
                            new_price = changes.i[changes.i$plan_audit_id == max(changes.i$plan_audit_id), "plan_price"]
                            
                            paste(old_price, "¢", " -> ", new_price, "¢", sep = "")
                            
                        } else {
                            paste0(" ")
                        }
                        
                    }
                    
                } else{
                    paste0(" ")
                }
                
                
            })
            
            
            
            
            
            
            output$change.1 <- renderInfoBox({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                leads <- get.causal.impact.data()[[7]]
                
                k <- 1
                
                change.date.i <- max(changes[changes$change_index == top.impacts.leads$change_index[k], "date"])
                
                product.i <- unique(changes[changes$change_index == top.impacts.leads$change_index[k], "product"])
                utility.i <- paste(unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility"]), unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility_id"]), sep = " - ")
                
                leads.ct %>%
                    filter(utility_name_id == utility.i) %>%
                    filter(product == product.i) -> leads.i
                
                leads.ts <- zoo(leads.i[,!names(leads.i) %in% c("date_submitted", "utility_name_id", "product")], order.by = leads.i$date_submitted)
                pre.i <- c(min(leads.i$date_submitted), change.date.i)
                post.i <- c(change.date.i + 1, max(leads.i$date_submitted))
                impact.i <- try.Causal.Impact(time_series = leads.ts, pre_date = pre.i, post_date = post.i, conf_int = 0.05)
                
                if(impact.i$summary$AbsEffect[1] < 0){
                    subt <- "Lead Volume Decline"
                    ico <- icon("arrow-down")
                } else {
                    subt <- "Lead Volume Increase"
                    ico <- icon("arrow-up")
                }
                
                
                infoBox(
                    "What occurred:", subt, icon = ico,
                    color = "purple"
                )
                
            })
            
            output$utility.1 <- renderInfoBox({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                
                k <- 1
                infoBox(
                    "Where:", paste(top.impacts.leads[k, c("state")], top.impacts.leads[k, c("utility")], sep = " - "), icon = icon("plug", "fa-0.5x"),
                    color = "green"
                )        
            })
            
            output$product.1 <- renderInfoBox({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                
                k <- 1
                if(top.impacts.leads[k, c("product")] == "Electricity"){
                    ico <- icon("bolt", "fa-0.5x")
                } else{
                    ico <- icon("fire", "fa-0.5x")
                }
                infoBox(
                    "Which product:", top.impacts.leads[k, c("product")], icon = ico,
                    color = "yellow"
                )        
            })
            
            
            output$causal.impact.cr.plot1 <- renderPlot({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                
                changes.unique <- changes[!duplicated(changes[,c("date", "utility_id")]),]
                unique.index <- unique(changes.unique$change_index)
                
                k <- 1
                
                change.date.i <- max(changes[changes$change_index == top.impacts.leads$change_index[k], "date"])
                
                product.i <- unique(changes[changes$change_index == top.impacts.leads$change_index[k], "product"])
                utility.i <- paste(unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility"]), unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility_id"]), sep = " - ")
                
                cr.ct %>%
                    filter(utility_name_id == utility.i) %>%
                    filter(product == product.i) -> leads.i
                
                leads.ts <- zoo(leads.i[,!names(leads.i) %in% c("date_submitted", "utility_name_id", "product")], order.by = leads.i$date_submitted)
                pre.i <- c(min(leads.i$date_submitted), change.date.i)
                post.i <- c(change.date.i + 1, max(leads.i$date_submitted))
                impact.i <- try.Causal.Impact(time_series = leads.ts, pre_date = pre.i, post_date = post.i, conf_int = 0.05)
                plot(impact.i)
            })
            
            
            output$causal.impact.cr.summary.EXCLUDE <- renderPrint({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                
                changes.unique <- changes[!duplicated(changes[,c("date", "utility_id")]),]
                unique.index <- unique(changes.unique$change_index)
                
                k <- 1
                
                change.date.i <- max(changes[changes$change_index == top.impacts.leads$change_index[k], "date"])
                
                product.i <- unique(changes[changes$change_index == top.impacts.leads$change_index[k], "product"])
                utility.i <- paste(unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility"]), unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility_id"]), sep = " - ")
                
                cr.ct %>%
                    filter(utility_name_id == utility.i) %>%
                    filter(product == product.i) -> leads.i
                
                leads.ts <- zoo(leads.i[,!names(leads.i) %in% c("date_submitted", "utility_name_id", "product")], order.by = leads.i$date_submitted)
                pre.i <- c(min(leads.i$date_submitted), change.date.i)
                post.i <- c(change.date.i + 1, max(leads.i$date_submitted))
                impact.i <- try.Causal.Impact(time_series = leads.ts, pre_date = pre.i, post_date = post.i, conf_int = 0.05)
                summary(impact.i)
            })        
            
            
            output$causal.impact.cr.summary1 <- renderPrint({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                
                changes.unique <- changes[!duplicated(changes[,c("date", "utility_id")]),]
                unique.index <- unique(changes.unique$change_index)
                
                k <- 1
                
                change.date.i <- max(changes[changes$change_index == top.impacts.leads$change_index[k], "date"])
                
                product.i <- unique(changes[changes$change_index == top.impacts.leads$change_index[k], "product"])
                utility.i <- paste(unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility"]), unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility_id"]), sep = " - ")
                cr.ct %>%
                    filter(utility_name_id == utility.i) %>%
                    filter(product == product.i) -> leads.i
                
                leads.ts <- zoo(leads.i[,!names(leads.i) %in% c("date_submitted", "utility_name_id", "product")], order.by = leads.i$date_submitted)
                pre.i <- c(min(leads.i$date_submitted), change.date.i)
                post.i <- c(change.date.i + 1, max(leads.i$date_submitted))
                impact.i <- try.Causal.Impact(time_series = leads.ts, pre_date = pre.i, post_date = post.i, conf_int = 0.05)
                summary(impact.i)
            })
            
            output$causal.impact.leads.summary1 <- renderPrint({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                
                changes.unique <- changes[!duplicated(changes[,c("date", "utility_id")]),]
                unique.index <- unique(changes.unique$change_index)
                
                k <- 1
                
                change.date.i <- max(changes[changes$change_index == top.impacts.leads$change_index[k], "date"])
                
                product.i <- unique(changes[changes$change_index == top.impacts.leads$change_index[k], "product"])
                utility.i <- paste(unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility"]), unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility_id"]), sep = " - ")
                leads.ct %>%
                    filter(utility_name_id == utility.i) %>%
                    filter(product == product.i) -> leads.i
                
                leads.ts <- zoo(leads.i[,!names(leads.i) %in% c("date_submitted", "utility_name_id", "product")], order.by = leads.i$date_submitted)
                pre.i <- c(min(leads.i$date_submitted), change.date.i)
                post.i <- c(change.date.i + 1, max(leads.i$date_submitted))
                impact.i <- try.Causal.Impact(time_series = leads.ts, pre_date = pre.i, post_date = post.i, conf_int = 0.05)
                summary(impact.i)
            })
            
            
            output$causal.impact.leads.plot1 <- renderPlot({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                
                changes.unique <- changes[!duplicated(changes[,c("date", "utility_id")]),]
                unique.index <- unique(changes.unique$change_index)
                
                k <- 1
                
                change.date.i <- max(changes[changes$change_index == top.impacts.leads$change_index[k], "date"])
                
                product.i <- unique(changes[changes$change_index == top.impacts.leads$change_index[k], "product"])
                utility.i <- paste(unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility"]), unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility_id"]), sep = " - ")
                leads.ct %>%
                    filter(utility_name_id == utility.i) %>%
                    filter(product == product.i) -> leads.i
                
                leads.ts <- zoo(leads.i[,!names(leads.i) %in% c("date_submitted", "utility_name_id", "product")], order.by = leads.i$date_submitted)
                pre.i <- c(min(leads.i$date_submitted), change.date.i)
                post.i <- c(change.date.i + 1, max(leads.i$date_submitted))
                impact.i <- try.Causal.Impact(time_series = leads.ts, pre_date = pre.i, post_date = post.i, conf_int = 0.05)
                plot(impact.i)
            })
            
            
            output$complete.plan.changes1 <- renderPlotly({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                
                changes.unique <- changes[!duplicated(changes[,c("date", "utility_id")]),]
                unique.index <- unique(changes.unique$change_index)
                
                k <- 1
                
                change.date.i <- max(changes[changes$change_index == top.impacts.leads$change_index[k], "date"])
                
                product.i <- unique(changes[changes$change_index == top.impacts.leads$change_index[k], "product"])
                utility.i <- paste(unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility"]), unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility_id"]), sep = " - ")
                
                utility.name.i <- unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility_name_id"])
                changes[changes$utility_name_id == utility.name.i, ] %>%
                    filter(graph.date > (today()-(daysback+1))) %>%
                    mutate(Supplier_Plan = paste(change, supplier, plan_name, sep = " - ")) -> changes.full.i
                
                daily.prices %>%
                    filter(utility_name_id == utility.i) %>%
                    filter(product == product.i) -> daily.prices.i
                
                
                
                p <- ggplot() + 
                    geom_line(data = daily.prices.i, aes(date, plan_price.mean)) +
                    geom_line(data = daily.prices.i, aes(date, plan_price.max)) +
                    geom_line(data = daily.prices.i, aes(date, plan_price.min)) +
                    geom_line(data = changes.full.i[changes.full.i$change == "plan_change",], aes(graph.date, plan_price, col = Supplier_Plan)) +
                    geom_point(data = changes.full.i[changes.full.i$change == "plan_end",], aes(graph.date, plan_price, col = Supplier_Plan)) + 
                    geom_point(data = changes.full.i[changes.full.i$change == "new_plan",], aes(graph.date, plan_price, col = Supplier_Plan)) +
                    geom_vline(xintercept = as.numeric(change.date.i), linetype = "longdash") +
                    ggtitle(paste("All Plan Changes Around", change.date.i, sep = " ")) +
                    xlab("Date") +
                    ylab("Plan Price (cents)") +
                    theme(legend.position = "none", panel.background=element_blank(), panel.grid.major = element_line(colour = "grey"))
                
                ggplotly(p)
            })
            
        
            
            
            output$plan_breakdown1 <- renderPlot({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                leads <- get.causal.impact.data()[[7]]
                sessions.ct <- get.causal.impact.data()[[8]]     
                plan.inv <- get.causal.impact.data()[[9]]
                
                
                changes.unique <- changes[!duplicated(changes[,c("date", "utility_id")]),]
                unique.index <- unique(changes.unique$change_index)
                
                k <- 1
                
                change.date.i <- max(changes[changes$change_index == top.impacts.leads$change_index[k], "date"])
                
                product.i <- unique(changes[changes$change_index == top.impacts.leads$change_index[k], "product"])
                utility.i <- paste(unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility"]), unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility_id"]), sep = " - ")
                
                utility.name.i <- unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility_name_id"])
                changes[changes$utility_name_id == utility.name.i, ] %>%
                    filter(graph.date > (today()-(daysback+1))) %>%
                    mutate(suplan = paste(change, supplier, plan_name, sep = " - ")) -> changes.full.i
                
                daily.prices %>%
                    filter(utility_name_id == utility.i) %>%
                    filter(product == product.i) -> daily.prices.i
                
                
                leads %>%
                    mutate(n = 1) %>%
                    unite(original_plan_plus_utility_id, original_plan_id, utility_id, sep = "-") %>%
                    aggregate(n~original_plan_plus_utility_id + date_submitted, data=., sum) -> leads.by.plan_plus_utility_id     
                
                lbp.spread <- spread(leads.by.plan_plus_utility_id,key = original_plan_plus_utility_id, value = n, fill = 0)
                
                lbp.zoo <- zoo(lbp.spread[,-1], lbp.spread[,1])
                
                merge(lbp.zoo, zoo(1, seq(today() - daysback, today(), by = 1)), all = TRUE) %>% 
                    .[,-(ncol(.))] %>%
                    fortify.zoo(.) %>%
                    reshape2::melt("Index") %>%
                    dplyr::rename(date_submitted = Index, original_plan_plus_utility_id = variable, n = value) %>%
                    separate(original_plan_plus_utility_id, into = c("original_plan_id", "utility_id"), sep = "-") -> lbp 
                
                
                
                til.i <- top.impacts.leads[k,]
                ticr.i <- top.impacts.cr[k,]
                
                if(between(til.i$Average_Relative_Effect, ticr.i$Avg_Rel_Eff_Lo_90_Conf_Interval, ticr.i$Avg_Rel_Eff_Up_90_Conf_Interval)){
                    
                    
                    leads.ct[leads.ct$utility_name_id == top.impacts.leads[k, "utility_name_id"],] %>%
                        filter(product == top.impacts.leads[k, "product"]) %>%
                        .[,!names(.) %in% c("utility_name_id", "product")] -> leads.i
                    
                    lbp %>%
                        filter(utility_id == top.impacts.leads[k, "utility_id"]) %>%
                        .[,!names(.) %in% c("utility_id")] %>%
                        spread(key = original_plan_id, value = n, fill = 0) -> lbp.i
                    
                    for.cor.i <- merge(leads.i, lbp.i, by = "date_submitted")
                    
                    arrange(reshape2::melt(cor(for.cor.i$n, for.cor.i[,-1:-2])),desc(value)) %>%
                        .[,!names(.) %in% c("Var1")] %>%
                        dplyr::rename(original_plan_id = Var2, cor = value) -> cor.i
                    
                    lbp.i %>%
                        .[,names(.) %in% c("date_submitted", cor.i$original_plan_id[1:5])] %>%
                        reshape2::melt("date_submitted") %>%
                        dplyr::rename(original_plan_id = variable, n = value) %>%
                        merge(unique(plan.inv[,c("supplier", "plan_name", "original_plan_id")]), by = "original_plan_id", all.x = TRUE) %>%
                        unite(suplan, supplier, plan_name, sep = " - ", remove = TRUE) %>%
                        .[,!names(.) %in% c("original_plan_id")] -> top.lbp
                    
                    changes %>%
                        filter(utility_name_id == top.impacts.leads[k, "utility_name_id"]) %>%
                        filter(original_plan_id == as.numeric(cor.i$original_plan_id[1])) -> changes.i
                    
                    changes.i <- arrange(changes.i[!duplicated(changes.i),], plan_audit_id)
                    
                    leads.i %>%
                        mutate(suplan = "Total Leads") %>%
                        .[,c("date_submitted", "n", "suplan")] %>%
                        rbind(top.lbp) -> leads.for.plot
                    
                    leads.for.plot$suplan <- as.character(leads.for.plot$suplan)
                    leads.for.plot$suplan <- factor(leads.for.plot$suplan, levels = c("Total Leads", unique(top.lbp$suplan)))
                    
                    names(leads.for.plot) <- c("date_submitted", "n", "suplan")
                    
                    ggplot(leads.for.plot, aes(date_submitted, n)) + 
                        geom_line() + 
                        facet_wrap(~suplan, ncol = 1) + 
                        ggtitle("Lead Volume of Top Correlating Plans") +
                        theme(legend.position = "none", panel.background=element_blank(), panel.grid.major = element_line(colour = "grey"), strip.text.y = element_text(angle=0), plot.title = element_text(hjust = 0.5, size = 18))+
                        xlab("Date") +
                        ylab("Lead Volume") +
                        geom_vline(xintercept = as.numeric(change.date.i), linetype = "longdash")
                    
                } else{
                    sessions.ct %>%
                        filter(utility_name_id == top.impacts.leads[k, "utility_name_id"]) %>%
                        aggregate(uniquePageviews ~ date_submitted, data=., sum) -> sessions.for.plot
                    
                    ggplot(sessions.for.plot, aes(date_submitted, uniquePageviews)) + 
                        geom_line() + 
                        ggtitle(paste("Sessions in ", top.impacts.leads[k, "utility"], sep = "")) +
                        theme(legend.position = "none", panel.background=element_blank(), panel.grid.major = element_line(colour = "grey")) +
                        xlab("Date") +
                        ylab("Sessions") +
                        geom_vline(xintercept = as.numeric(change.date.i), linetype = "longdash") +
                        theme(plot.title = element_text(hjust = 0.5, size = 18)) +
                        stat_smooth(method=loess)
                    
                }
                
                
            })
            
            
            # # # --- # # # --- # # # --- # # # --- # # # ---
            
            #                      TAB 2                    #  
            
            # # # --- # # # --- # # # --- # # # --- # # # ---
            
            
            
            
            
            
            
            output$likelihood.description.2 <- renderText({
                
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                leads <- get.causal.impact.data()[[7]]
                
                
                k <- 2
                
                change.date.i <- top.impacts.leads[k, "graph.date"]
                
                leads %>%
                    mutate(n = 1) %>%
                    unite(original_plan_plus_utility_id, original_plan_id, utility_id, sep = "-") %>%
                    aggregate(n~original_plan_plus_utility_id + date_submitted, data=., sum) -> leads.by.plan_plus_utility_id     
                
                lbp.spread <- spread(leads.by.plan_plus_utility_id,key = original_plan_plus_utility_id, value = n, fill = 0)
                
                lbp.zoo <- zoo(lbp.spread[,-1], lbp.spread[,1])
                
                merge(lbp.zoo, zoo(1, seq(today() - daysback, today(), by = 1)), all = TRUE) %>% 
                    .[,-(ncol(.))] %>%
                    fortify.zoo(.) %>%
                    reshape2::melt("Index") %>%
                    dplyr::rename(date_submitted = Index, original_plan_plus_utility_id = variable, n = value) %>%
                    separate(original_plan_plus_utility_id, into = c("original_plan_id", "utility_id"), sep = "-") -> lbp 
                
                
                til.i <- top.impacts.leads[k,]
                ticr.i <- top.impacts.cr[k,]
                
                if(between(til.i$Average_Relative_Effect, ticr.i$Avg_Rel_Eff_Lo_90_Conf_Interval, ticr.i$Avg_Rel_Eff_Up_90_Conf_Interval)){
                    
                    
                    leads.ct[leads.ct$utility_name_id == top.impacts.leads[k, "utility_name_id"],] %>%
                        filter(product == top.impacts.leads[k, "product"]) %>%
                        .[,!names(.) %in% c("utility_name_id", "product")] -> leads.i
                    
                    lbp %>%
                        filter(utility_id == top.impacts.leads[k, "utility_id"]) %>%
                        .[,!names(.) %in% c("utility_id")] %>%
                        spread(key = original_plan_id, value = n, fill = 0) -> lbp.i
                    
                    for.cor.i <- merge(leads.i, lbp.i, by = "date_submitted")
                    
                    arrange(reshape2::melt(cor(for.cor.i$n, for.cor.i[,-1:-2])),desc(value)) %>%
                        .[,!names(.) %in% c("Var1")] %>%
                        dplyr::rename(original_plan_id = Var2, cor = value) -> cor.i
                    
                    lbp.i %>%
                        .[,names(.) %in% c("date_submitted", cor.i$original_plan_id[1:5])] %>%
                        reshape2::melt("date_submitted") %>%
                        dplyr::rename(original_plan_id = variable, n = value) -> top.lbp
                    
                    changes %>%
                        filter(utility_name_id == top.impacts.leads[k, "utility_name_id"]) %>%
                        filter(original_plan_id == as.numeric(cor.i$original_plan_id[1])) -> changes.i
                    
                    changes.i <- arrange(changes.i[!duplicated(changes.i),], plan_audit_id)
                    
                    if(dim(changes.i)[1] == 0){
                        
                        description <- paste0("Both lead volume and conversion rate were down, although no particular plan changes occurred that could be traced back to. See Lead Volume of Top Correlating Plans below for possibile causes.")
                        
                    } else {
                        if(between(changes.i[1,"date"], change.date.i - 3, change.date.i + 3)){
                            
                            description <- paste0("This plan change is highly likely to have caused the observed change in lead and conversion rate.")
                            
                        } else {
                            description <- paste0("Both lead volume and conversion rate were down, although no particular plan changes occurred that could be traced back to. See Lead Volume of Top Correlating Plans below for possibile causes.")
                        }
                    }
                    
                } else{
                    description <- paste0("No plan changes were found relating to this impact, and conversion rate either stayed relatively the same or went up while leads went down. Therefore the impact can be traced back to a change in traffic in this utility zone.")
                }
                
                description
                
            })
            
            
            
            
            
            output$plan_name2.tab <- renderText({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                leads <- get.causal.impact.data()[[7]]
                
                
                k <- 2
                
                change.date.i <- top.impacts.leads[k, "graph.date"]
                
                leads %>%
                    mutate(n = 1) %>%
                    unite(original_plan_plus_utility_id, original_plan_id, utility_id, sep = "-") %>%
                    aggregate(n~original_plan_plus_utility_id + date_submitted, data=., sum) -> leads.by.plan_plus_utility_id     
                
                lbp.spread <- spread(leads.by.plan_plus_utility_id,key = original_plan_plus_utility_id, value = n, fill = 0)
                
                lbp.zoo <- zoo(lbp.spread[,-1], lbp.spread[,1])
                
                merge(lbp.zoo, zoo(1, seq(today() - daysback, today(), by = 1)), all = TRUE) %>% 
                    .[,-(ncol(.))] %>%
                    fortify.zoo(.) %>%
                    reshape2::melt("Index") %>%
                    dplyr::rename(date_submitted = Index, original_plan_plus_utility_id = variable, n = value) %>%
                    separate(original_plan_plus_utility_id, into = c("original_plan_id", "utility_id"), sep = "-") -> lbp 
                
                
                
                
                
                til.i <- top.impacts.leads[k,]
                ticr.i <- top.impacts.cr[k,]
                
                if(between(til.i$Average_Relative_Effect, ticr.i$Avg_Rel_Eff_Lo_90_Conf_Interval, ticr.i$Avg_Rel_Eff_Up_90_Conf_Interval)){
                    
                    
                    leads.ct[leads.ct$utility_name_id == top.impacts.leads[k, "utility_name_id"],] %>%
                        filter(product == top.impacts.leads[k, "product"]) %>%
                        .[,!names(.) %in% c("utility_name_id", "product")] -> leads.i
                    
                    lbp %>%
                        filter(utility_id == top.impacts.leads[k, "utility_id"]) %>%
                        .[,!names(.) %in% c("utility_id")] %>%
                        spread(key = original_plan_id, value = n, fill = 0) -> lbp.i
                    
                    for.cor.i <- merge(leads.i, lbp.i, by = "date_submitted")
                    
                    arrange(reshape2::melt(cor(for.cor.i$n, for.cor.i[,-1:-2])),desc(value)) %>%
                        .[,!names(.) %in% c("Var1")] %>%
                        dplyr::rename(original_plan_id = Var2, cor = value) -> cor.i
                    
                    lbp.i %>%
                        .[,names(.) %in% c("date_submitted", cor.i$original_plan_id[1:5])] %>%
                        reshape2::melt("date_submitted") %>%
                        dplyr::rename(original_plan_id = variable, n = value) -> top.lbp
                    
                    changes %>%
                        filter(utility_name_id == top.impacts.leads[k, "utility_name_id"]) %>%
                        filter(original_plan_id == as.numeric(cor.i$original_plan_id[1])) -> changes.i
                    
                    changes.i <- arrange(changes.i[!duplicated(changes.i),], plan_audit_id)
                    
                    if(dim(changes.i)[1] == 0){
                        
                        paste0("Multiple Possibilities")
                        
                    } else {
                        if(between(changes.i[1,"date"], change.date.i - 3, change.date.i + 3)){
                            
                            as.character(changes.i[1, c("plan_name")])
                            
                        } else {
                            paste0("Multiple Possibilities")
                        }
                        
                    }
                    
                } else{
                    paste0("Traffic Change")
                }
                
                
                
            })
            
            
            output$plan_term2.tab <- renderText({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                leads <- get.causal.impact.data()[[7]]
                
                
                k <- 2
                
                change.date.i <- top.impacts.leads[k, "graph.date"]
                
                leads %>%
                    mutate(n = 1) %>%
                    unite(original_plan_plus_utility_id, original_plan_id, utility_id, sep = "-") %>%
                    aggregate(n~original_plan_plus_utility_id + date_submitted, data=., sum) -> leads.by.plan_plus_utility_id     
                
                lbp.spread <- spread(leads.by.plan_plus_utility_id,key = original_plan_plus_utility_id, value = n, fill = 0)
                
                lbp.zoo <- zoo(lbp.spread[,-1], lbp.spread[,1])
                
                merge(lbp.zoo, zoo(1, seq(today() - daysback, today(), by = 1)), all = TRUE) %>% 
                    .[,-(ncol(.))] %>%
                    fortify.zoo(.) %>%
                    reshape2::melt("Index") %>%
                    dplyr::rename(date_submitted = Index, original_plan_plus_utility_id = variable, n = value) %>%
                    separate(original_plan_plus_utility_id, into = c("original_plan_id", "utility_id"), sep = "-") -> lbp 
                
                
                
                til.i <- top.impacts.leads[k,]
                ticr.i <- top.impacts.cr[k,]
                
                if(between(til.i$Average_Relative_Effect, ticr.i$Avg_Rel_Eff_Lo_90_Conf_Interval, ticr.i$Avg_Rel_Eff_Up_90_Conf_Interval)){
                    
                    
                    leads.ct[leads.ct$utility_name_id == top.impacts.leads[k, "utility_name_id"],] %>%
                        filter(product == top.impacts.leads[k, "product"]) %>%
                        .[,!names(.) %in% c("utility_name_id", "product")] -> leads.i
                    
                    lbp %>%
                        filter(utility_id == top.impacts.leads[k, "utility_id"]) %>%
                        .[,!names(.) %in% c("utility_id")] %>%
                        spread(key = original_plan_id, value = n, fill = 0) -> lbp.i
                    
                    for.cor.i <- merge(leads.i, lbp.i, by = "date_submitted")
                    
                    arrange(reshape2::melt(cor(for.cor.i$n, for.cor.i[,-1:-2])),desc(value)) %>%
                        .[,!names(.) %in% c("Var1")] %>%
                        dplyr::rename(original_plan_id = Var2, cor = value) -> cor.i
                    
                    lbp.i %>%
                        .[,names(.) %in% c("date_submitted", cor.i$original_plan_id[1:5])] %>%
                        reshape2::melt("date_submitted") %>%
                        dplyr::rename(original_plan_id = variable, n = value) -> top.lbp
                    
                    changes %>%
                        filter(utility_name_id == top.impacts.leads[k, "utility_name_id"]) %>%
                        filter(original_plan_id == as.numeric(cor.i$original_plan_id[1])) -> changes.i
                    
                    changes.i <- arrange(changes.i[!duplicated(changes.i),], plan_audit_id)
                    
                    if(dim(changes.i)[1] == 0){
                        
                        paste0(" ")
                        
                    } else {
                        if(between(changes.i[1,"date"], change.date.i - 3, change.date.i + 3)){
                            
                            as.character(paste(changes.i[1, c("contract_term")], " Months ", changes.i[1, c("contract_type")], sep = ""))
                            
                        } else {
                            paste0(" ")
                        }
                        
                    }
                    
                } else{
                    paste0(" ")
                }
                
            }) 
            
            
            output$plan_supplier2.tab <- renderText({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                leads <- get.causal.impact.data()[[7]]
                
                
                k <- 2
                
                change.date.i <- top.impacts.leads[k, "graph.date"]
                
                leads %>%
                    mutate(n = 1) %>%
                    unite(original_plan_plus_utility_id, original_plan_id, utility_id, sep = "-") %>%
                    aggregate(n~original_plan_plus_utility_id + date_submitted, data=., sum) -> leads.by.plan_plus_utility_id     
                
                lbp.spread <- spread(leads.by.plan_plus_utility_id,key = original_plan_plus_utility_id, value = n, fill = 0)
                
                lbp.zoo <- zoo(lbp.spread[,-1], lbp.spread[,1])
                
                merge(lbp.zoo, zoo(1, seq(today() - daysback, today(), by = 1)), all = TRUE) %>% 
                    .[,-(ncol(.))] %>%
                    fortify.zoo(.) %>%
                    reshape2::melt("Index") %>%
                    dplyr::rename(date_submitted = Index, original_plan_plus_utility_id = variable, n = value) %>%
                    separate(original_plan_plus_utility_id, into = c("original_plan_id", "utility_id"), sep = "-") -> lbp 
                
                
                
                til.i <- top.impacts.leads[k,]
                ticr.i <- top.impacts.cr[k,]
                
                if(between(til.i$Average_Relative_Effect, ticr.i$Avg_Rel_Eff_Lo_90_Conf_Interval, ticr.i$Avg_Rel_Eff_Up_90_Conf_Interval)){
                    
                    
                    leads.ct[leads.ct$utility_name_id == top.impacts.leads[k, "utility_name_id"],] %>%
                        filter(product == top.impacts.leads[k, "product"]) %>%
                        .[,!names(.) %in% c("utility_name_id", "product")] -> leads.i
                    
                    lbp %>%
                        filter(utility_id == top.impacts.leads[k, "utility_id"]) %>%
                        .[,!names(.) %in% c("utility_id")] %>%
                        spread(key = original_plan_id, value = n, fill = 0) -> lbp.i
                    
                    for.cor.i <- merge(leads.i, lbp.i, by = "date_submitted")
                    
                    arrange(reshape2::melt(cor(for.cor.i$n, for.cor.i[,-1:-2])),desc(value)) %>%
                        .[,!names(.) %in% c("Var1")] %>%
                        dplyr::rename(original_plan_id = Var2, cor = value) -> cor.i
                    
                    lbp.i %>%
                        .[,names(.) %in% c("date_submitted", cor.i$original_plan_id[1:5])] %>%
                        reshape2::melt("date_submitted") %>%
                        dplyr::rename(original_plan_id = variable, n = value) -> top.lbp
                    
                    changes %>%
                        filter(utility_name_id == top.impacts.leads[k, "utility_name_id"]) %>%
                        filter(original_plan_id == as.numeric(cor.i$original_plan_id[1])) -> changes.i
                    
                    changes.i <- arrange(changes.i[!duplicated(changes.i),], plan_audit_id)
                    
                    if(dim(changes.i)[1] == 0){
                        
                        paste0(" ")
                        
                    } else {
                        if(between(changes.i[1,"date"], change.date.i - 3, change.date.i + 3)){
                            
                            as.character(changes.i[1, c("supplier")])
                            
                        } else {
                            paste0(" ")
                        }
                        
                    }
                    
                } else{
                    paste0(" ")
                }
                
            })
            
            
            output$plan_active2.tab <- renderText({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                leads <- get.causal.impact.data()[[7]]
                
                
                k <- 2
                
                change.date.i <- top.impacts.leads[k, "graph.date"]
                
                leads %>%
                    mutate(n = 1) %>%
                    unite(original_plan_plus_utility_id, original_plan_id, utility_id, sep = "-") %>%
                    aggregate(n~original_plan_plus_utility_id + date_submitted, data=., sum) -> leads.by.plan_plus_utility_id     
                
                lbp.spread <- spread(leads.by.plan_plus_utility_id,key = original_plan_plus_utility_id, value = n, fill = 0)
                
                lbp.zoo <- zoo(lbp.spread[,-1], lbp.spread[,1])
                
                merge(lbp.zoo, zoo(1, seq(today() - daysback, today(), by = 1)), all = TRUE) %>% 
                    .[,-(ncol(.))] %>%
                    fortify.zoo(.) %>%
                    reshape2::melt("Index") %>%
                    dplyr::rename(date_submitted = Index, original_plan_plus_utility_id = variable, n = value) %>%
                    separate(original_plan_plus_utility_id, into = c("original_plan_id", "utility_id"), sep = "-") -> lbp 
                
                
                
                til.i <- top.impacts.leads[k,]
                ticr.i <- top.impacts.cr[k,]
                
                if(between(til.i$Average_Relative_Effect, ticr.i$Avg_Rel_Eff_Lo_90_Conf_Interval, ticr.i$Avg_Rel_Eff_Up_90_Conf_Interval)){
                    
                    
                    leads.ct[leads.ct$utility_name_id == top.impacts.leads[k, "utility_name_id"],] %>%
                        filter(product == top.impacts.leads[k, "product"]) %>%
                        .[,!names(.) %in% c("utility_name_id", "product")] -> leads.i
                    
                    lbp %>%
                        filter(utility_id == top.impacts.leads[k, "utility_id"]) %>%
                        .[,!names(.) %in% c("utility_id")] %>%
                        spread(key = original_plan_id, value = n, fill = 0) -> lbp.i
                    
                    for.cor.i <- merge(leads.i, lbp.i, by = "date_submitted")
                    
                    arrange(reshape2::melt(cor(for.cor.i$n, for.cor.i[,-1:-2])),desc(value)) %>%
                        .[,!names(.) %in% c("Var1")] %>%
                        dplyr::rename(original_plan_id = Var2, cor = value) -> cor.i
                    
                    lbp.i %>%
                        .[,names(.) %in% c("date_submitted", cor.i$original_plan_id[1:5])] %>%
                        reshape2::melt("date_submitted") %>%
                        dplyr::rename(original_plan_id = variable, n = value) -> top.lbp
                    
                    changes %>%
                        filter(utility_name_id == top.impacts.leads[k, "utility_name_id"]) %>%
                        filter(original_plan_id == as.numeric(cor.i$original_plan_id[1])) -> changes.i
                    
                    changes.i <- arrange(changes.i[!duplicated(changes.i),], plan_audit_id)
                    
                    if(dim(changes.i)[1] == 0){
                        
                        paste0(" ")
                        
                    } else {
                        if(between(changes.i[1,"date"], change.date.i - 3, change.date.i + 3)){
                            
                            new_plan_status = changes.i[changes.i$plan_audit_id == max(changes.i$plan_audit_id), "plan_status_name"]
                            
                            new_plan_status
                            
                        } else {
                            paste0(" ")
                        }
                        
                    }
                    
                } else{
                    paste0(" ")
                }
                
            })
            
            output$plan_id2.tab <- renderText({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                leads <- get.causal.impact.data()[[7]]
                
                
                k <- 2
                
                change.date.i <- top.impacts.leads[k, "graph.date"]
                
                leads %>%
                    mutate(n = 1) %>%
                    unite(original_plan_plus_utility_id, original_plan_id, utility_id, sep = "-") %>%
                    aggregate(n~original_plan_plus_utility_id + date_submitted, data=., sum) -> leads.by.plan_plus_utility_id     
                
                lbp.spread <- spread(leads.by.plan_plus_utility_id,key = original_plan_plus_utility_id, value = n, fill = 0)
                
                lbp.zoo <- zoo(lbp.spread[,-1], lbp.spread[,1])
                
                merge(lbp.zoo, zoo(1, seq(today() - daysback, today(), by = 1)), all = TRUE) %>% 
                    .[,-(ncol(.))] %>%
                    fortify.zoo(.) %>%
                    reshape2::melt("Index") %>%
                    dplyr::rename(date_submitted = Index, original_plan_plus_utility_id = variable, n = value) %>%
                    separate(original_plan_plus_utility_id, into = c("original_plan_id", "utility_id"), sep = "-") -> lbp 
                
                
                
                til.i <- top.impacts.leads[k,]
                ticr.i <- top.impacts.cr[k,]
                
                if(between(til.i$Average_Relative_Effect, ticr.i$Avg_Rel_Eff_Lo_90_Conf_Interval, ticr.i$Avg_Rel_Eff_Up_90_Conf_Interval)){
                    
                    
                    leads.ct[leads.ct$utility_name_id == top.impacts.leads[k, "utility_name_id"],] %>%
                        filter(product == top.impacts.leads[k, "product"]) %>%
                        .[,!names(.) %in% c("utility_name_id", "product")] -> leads.i
                    
                    lbp %>%
                        filter(utility_id == top.impacts.leads[k, "utility_id"]) %>%
                        .[,!names(.) %in% c("utility_id")] %>%
                        spread(key = original_plan_id, value = n, fill = 0) -> lbp.i
                    
                    for.cor.i <- merge(leads.i, lbp.i, by = "date_submitted")
                    
                    arrange(reshape2::melt(cor(for.cor.i$n, for.cor.i[,-1:-2])),desc(value)) %>%
                        .[,!names(.) %in% c("Var1")] %>%
                        dplyr::rename(original_plan_id = Var2, cor = value) -> cor.i
                    
                    lbp.i %>%
                        .[,names(.) %in% c("date_submitted", cor.i$original_plan_id[1:5])] %>%
                        reshape2::melt("date_submitted") %>%
                        dplyr::rename(original_plan_id = variable, n = value) -> top.lbp
                    
                    changes %>%
                        filter(utility_name_id == top.impacts.leads[k, "utility_name_id"]) %>%
                        filter(original_plan_id == as.numeric(cor.i$original_plan_id[1])) -> changes.i
                    
                    changes.i <- arrange(changes.i[!duplicated(changes.i),], plan_audit_id)
                    
                    
                    if(dim(changes.i)[1] == 0){
                        
                        paste0(" ")
                        
                    } else {
                        if(between(changes.i[1,"date"], change.date.i - 3, change.date.i + 3)){
                            
                            as.character(paste("Plan ID: ", changes.i[1, c("original_plan_id")], sep = ""))
                            
                        } else {
                            paste0(" ")
                        }
                        
                    }
                    
                } else{
                    paste0(" ")
                }
                
                
            })
            
            
            
            output$price_change2 <- renderText({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                leads <- get.causal.impact.data()[[7]]
                
                
                k <- 2
                
                change.date.i <- top.impacts.leads[k, "graph.date"]
                
                leads %>%
                    mutate(n = 1) %>%
                    unite(original_plan_plus_utility_id, original_plan_id, utility_id, sep = "-") %>%
                    aggregate(n~original_plan_plus_utility_id + date_submitted, data=., sum) -> leads.by.plan_plus_utility_id     
                
                lbp.spread <- spread(leads.by.plan_plus_utility_id,key = original_plan_plus_utility_id, value = n, fill = 0)
                
                lbp.zoo <- zoo(lbp.spread[,-1], lbp.spread[,1])
                
                merge(lbp.zoo, zoo(1, seq(today() - daysback, today(), by = 1)), all = TRUE) %>% 
                    .[,-(ncol(.))] %>%
                    fortify.zoo(.) %>%
                    reshape2::melt("Index") %>%
                    dplyr::rename(date_submitted = Index, original_plan_plus_utility_id = variable, n = value) %>%
                    separate(original_plan_plus_utility_id, into = c("original_plan_id", "utility_id"), sep = "-") -> lbp 
                
                
                
                til.i <- top.impacts.leads[k,]
                ticr.i <- top.impacts.cr[k,]
                
                if(between(til.i$Average_Relative_Effect, ticr.i$Avg_Rel_Eff_Lo_90_Conf_Interval, ticr.i$Avg_Rel_Eff_Up_90_Conf_Interval)){
                    
                    
                    leads.ct[leads.ct$utility_name_id == top.impacts.leads[k, "utility_name_id"],] %>%
                        filter(product == top.impacts.leads[k, "product"]) %>%
                        .[,!names(.) %in% c("utility_name_id", "product")] -> leads.i
                    
                    lbp %>%
                        filter(utility_id == top.impacts.leads[k, "utility_id"]) %>%
                        .[,!names(.) %in% c("utility_id")] %>%
                        spread(key = original_plan_id, value = n, fill = 0) -> lbp.i
                    
                    for.cor.i <- merge(leads.i, lbp.i, by = "date_submitted")
                    
                    arrange(reshape2::melt(cor(for.cor.i$n, for.cor.i[,-1:-2])),desc(value)) %>%
                        .[,!names(.) %in% c("Var1")] %>%
                        dplyr::rename(original_plan_id = Var2, cor = value) -> cor.i
                    
                    lbp.i %>%
                        .[,names(.) %in% c("date_submitted", cor.i$original_plan_id[1:5])] %>%
                        reshape2::melt("date_submitted") %>%
                        dplyr::rename(original_plan_id = variable, n = value) -> top.lbp
                    
                    changes %>%
                        filter(utility_name_id == top.impacts.leads[k, "utility_name_id"]) %>%
                        filter(original_plan_id == as.numeric(cor.i$original_plan_id[1])) -> changes.i
                    
                    changes.i <- arrange(changes.i[!duplicated(changes.i),], plan_audit_id)
                    
                    
                    
                    if(dim(changes.i)[1] == 0){
                        
                        paste0(" ")
                        
                    } else {
                        if(between(changes.i[1,"date"], change.date.i - 3, change.date.i + 3)){
                            
                            old_price = changes.i[changes.i$plan_audit_id == min(changes.i$plan_audit_id), "plan_price"]
                            new_price = changes.i[changes.i$plan_audit_id == max(changes.i$plan_audit_id), "plan_price"]
                            
                            paste(old_price, "¢", " -> ", new_price, "¢", sep = "")
                            
                        } else {
                            paste0(" ")
                        }
                        
                    }
                    
                } else{
                    paste0(" ")
                }
                
                
            })
            
            
            
            
            
            
            output$change.2 <- renderInfoBox({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                leads <- get.causal.impact.data()[[7]]
                
                k <- 2
                
                change.date.i <- max(changes[changes$change_index == top.impacts.leads$change_index[k], "date"])
                
                product.i <- unique(changes[changes$change_index == top.impacts.leads$change_index[k], "product"])
                utility.i <- paste(unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility"]), unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility_id"]), sep = " - ")
                
                leads.ct %>%
                    filter(utility_name_id == utility.i) %>%
                    filter(product == product.i) -> leads.i
                
                leads.ts <- zoo(leads.i[,!names(leads.i) %in% c("date_submitted", "utility_name_id", "product")], order.by = leads.i$date_submitted)
                pre.i <- c(min(leads.i$date_submitted), change.date.i)
                post.i <- c(change.date.i + 1, max(leads.i$date_submitted))
                impact.i <- try.Causal.Impact(time_series = leads.ts, pre_date = pre.i, post_date = post.i, conf_int = 0.05)
                
                if(impact.i$summary$AbsEffect[1] < 0){
                    subt <- "Lead Volume Decline"
                    ico <- icon("arrow-down")
                } else {
                    subt <- "Lead Volume Increase"
                    ico <- icon("arrow-up")
                }
                
                infoBox(
                    "What occurred:", subt, icon = ico,
                    color = "purple"
                )
                
            })
            
            output$utility.2 <- renderInfoBox({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                
                k <- 2
                infoBox(
                    "Where:", paste(top.impacts.leads[k, c("state")], top.impacts.leads[k, c("utility")], sep = " - "), icon = icon("plug", "fa-0.5x"),
                    color = "green"
                )        
            })
            
            output$product.2 <- renderInfoBox({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                
                k <- 2
                if(top.impacts.leads[k, c("product")] == "Electricity"){
                    ico <- icon("bolt", "fa-0.5x")
                } else{
                    ico <- icon("fire", "fa-0.5x")
                }
                infoBox(
                    "Which product:", top.impacts.leads[k, c("product")], icon = ico,
                    color = "yellow"
                )        
            })
            
            
            output$causal.impact.cr.plot2 <- renderPlot({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                
                changes.unique <- changes[!duplicated(changes[,c("date", "utility_id")]),]
                unique.index <- unique(changes.unique$change_index)
                
                k <- 2
                
                change.date.i <- max(changes[changes$change_index == top.impacts.leads$change_index[k], "date"])
                
                product.i <- unique(changes[changes$change_index == top.impacts.leads$change_index[k], "product"])
                utility.i <- paste(unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility"]), unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility_id"]), sep = " - ")
                
                cr.ct %>%
                    filter(utility_name_id == utility.i) %>%
                    filter(product == product.i) -> leads.i
                
                leads.ts <- zoo(leads.i[,!names(leads.i) %in% c("date_submitted", "utility_name_id", "product")], order.by = leads.i$date_submitted)
                pre.i <- c(min(leads.i$date_submitted), change.date.i)
                post.i <- c(change.date.i + 1, max(leads.i$date_submitted))
                impact.i <- try.Causal.Impact(time_series = leads.ts, pre_date = pre.i, post_date = post.i, conf_int = 0.05)
                plot(impact.i)
            })
            
            
            output$causal.impact.cr.summary.EXCLUDE <- renderPrint({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                
                changes.unique <- changes[!duplicated(changes[,c("date", "utility_id")]),]
                unique.index <- unique(changes.unique$change_index)
                
                k <- 2
                
                change.date.i <- max(changes[changes$change_index == top.impacts.leads$change_index[k], "date"])
                
                product.i <- unique(changes[changes$change_index == top.impacts.leads$change_index[k], "product"])
                utility.i <- paste(unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility"]), unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility_id"]), sep = " - ")
                
                cr.ct %>%
                    filter(utility_name_id == utility.i) %>%
                    filter(product == product.i) -> leads.i
                
                leads.ts <- zoo(leads.i[,!names(leads.i) %in% c("date_submitted", "utility_name_id", "product")], order.by = leads.i$date_submitted)
                pre.i <- c(min(leads.i$date_submitted), change.date.i)
                post.i <- c(change.date.i + 1, max(leads.i$date_submitted))
                impact.i <- try.Causal.Impact(time_series = leads.ts, pre_date = pre.i, post_date = post.i, conf_int = 0.05)
                summary(impact.i)
            })        
            
            
            output$causal.impact.cr.summary2 <- renderPrint({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                
                changes.unique <- changes[!duplicated(changes[,c("date", "utility_id")]),]
                unique.index <- unique(changes.unique$change_index)
                
                k <- 2
                
                change.date.i <- max(changes[changes$change_index == top.impacts.leads$change_index[k], "date"])
                
                product.i <- unique(changes[changes$change_index == top.impacts.leads$change_index[k], "product"])
                utility.i <- paste(unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility"]), unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility_id"]), sep = " - ")
                cr.ct %>%
                    filter(utility_name_id == utility.i) %>%
                    filter(product == product.i) -> leads.i
                
                leads.ts <- zoo(leads.i[,!names(leads.i) %in% c("date_submitted", "utility_name_id", "product")], order.by = leads.i$date_submitted)
                pre.i <- c(min(leads.i$date_submitted), change.date.i)
                post.i <- c(change.date.i + 1, max(leads.i$date_submitted))
                impact.i <- try.Causal.Impact(time_series = leads.ts, pre_date = pre.i, post_date = post.i, conf_int = 0.05)
                summary(impact.i)
            })
            
            output$causal.impact.leads.summary2 <- renderPrint({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                
                changes.unique <- changes[!duplicated(changes[,c("date", "utility_id")]),]
                unique.index <- unique(changes.unique$change_index)
                
                k <- 2
                
                change.date.i <- max(changes[changes$change_index == top.impacts.leads$change_index[k], "date"])
                
                product.i <- unique(changes[changes$change_index == top.impacts.leads$change_index[k], "product"])
                utility.i <- paste(unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility"]), unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility_id"]), sep = " - ")
                leads.ct %>%
                    filter(utility_name_id == utility.i) %>%
                    filter(product == product.i) -> leads.i
                
                leads.ts <- zoo(leads.i[,!names(leads.i) %in% c("date_submitted", "utility_name_id", "product")], order.by = leads.i$date_submitted)
                pre.i <- c(min(leads.i$date_submitted), change.date.i)
                post.i <- c(change.date.i + 1, max(leads.i$date_submitted))
                impact.i <- try.Causal.Impact(time_series = leads.ts, pre_date = pre.i, post_date = post.i, conf_int = 0.05)
                summary(impact.i)
            })
            
            
            output$causal.impact.leads.plot2 <- renderPlot({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                
                changes.unique <- changes[!duplicated(changes[,c("date", "utility_id")]),]
                unique.index <- unique(changes.unique$change_index)
                
                k <- 2
                
                change.date.i <- max(changes[changes$change_index == top.impacts.leads$change_index[k], "date"])
                
                product.i <- unique(changes[changes$change_index == top.impacts.leads$change_index[k], "product"])
                utility.i <- paste(unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility"]), unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility_id"]), sep = " - ")
                leads.ct %>%
                    filter(utility_name_id == utility.i) %>%
                    filter(product == product.i) -> leads.i
                
                leads.ts <- zoo(leads.i[,!names(leads.i) %in% c("date_submitted", "utility_name_id", "product")], order.by = leads.i$date_submitted)
                pre.i <- c(min(leads.i$date_submitted), change.date.i)
                post.i <- c(change.date.i + 1, max(leads.i$date_submitted))
                impact.i <- try.Causal.Impact(time_series = leads.ts, pre_date = pre.i, post_date = post.i, conf_int = 0.05)
                plot(impact.i)
            })
            
            
            output$complete.plan.changes2 <- renderPlotly({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                
                changes.unique <- changes[!duplicated(changes[,c("date", "utility_id")]),]
                unique.index <- unique(changes.unique$change_index)
                
                k <- 2
                
                change.date.i <- max(changes[changes$change_index == top.impacts.leads$change_index[k], "date"])
                
                product.i <- unique(changes[changes$change_index == top.impacts.leads$change_index[k], "product"])
                utility.i <- paste(unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility"]), unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility_id"]), sep = " - ")
                
                utility.name.i <- unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility_name_id"])
                changes[changes$utility_name_id == utility.name.i, ] %>%
                    filter(graph.date > (today()-(daysback+1))) %>%
                    mutate(Supplier_Plan = paste(change, supplier, plan_name, sep = " - ")) -> changes.full.i
                
                daily.prices %>%
                    filter(utility_name_id == utility.i) %>%
                    filter(product == product.i) -> daily.prices.i
                
                
                
                p <- ggplot() + 
                    geom_line(data = daily.prices.i, aes(date, plan_price.mean)) +
                    geom_line(data = daily.prices.i, aes(date, plan_price.max)) +
                    geom_line(data = daily.prices.i, aes(date, plan_price.min)) +
                    geom_line(data = changes.full.i[changes.full.i$change == "plan_change",], aes(graph.date, plan_price, col = Supplier_Plan)) +
                    geom_point(data = changes.full.i[changes.full.i$change == "plan_end",], aes(graph.date, plan_price, col = Supplier_Plan)) + 
                    geom_point(data = changes.full.i[changes.full.i$change == "new_plan",], aes(graph.date, plan_price, col = Supplier_Plan)) +
                    geom_vline(xintercept = as.numeric(change.date.i), linetype = "longdash") +
                    ggtitle(paste("All Plan Changes Around", change.date.i, sep = " ")) +
                    xlab("Date") +
                    ylab("Plan Price (cents)") +
                    theme(legend.position = "none", panel.background=element_blank(), panel.grid.major = element_line(colour = "grey"))
                
                ggplotly(p)
            })
            
            
            
            
            
            output$plan_breakdown2 <- renderPlot({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                leads <- get.causal.impact.data()[[7]]
                sessions.ct <- get.causal.impact.data()[[8]]     
                plan.inv <- get.causal.impact.data()[[9]]
                
                
                changes.unique <- changes[!duplicated(changes[,c("date", "utility_id")]),]
                unique.index <- unique(changes.unique$change_index)
                
                k <- 2
                
                change.date.i <- max(changes[changes$change_index == top.impacts.leads$change_index[k], "date"])
                
                product.i <- unique(changes[changes$change_index == top.impacts.leads$change_index[k], "product"])
                utility.i <- paste(unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility"]), unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility_id"]), sep = " - ")
                
                utility.name.i <- unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility_name_id"])
                changes[changes$utility_name_id == utility.name.i, ] %>%
                    filter(graph.date > (today()-(daysback+1))) %>%
                    mutate(suplan = paste(change, supplier, plan_name, sep = " - ")) -> changes.full.i
                
                daily.prices %>%
                    filter(utility_name_id == utility.i) %>%
                    filter(product == product.i) -> daily.prices.i
                
                
                leads %>%
                    mutate(n = 1) %>%
                    unite(original_plan_plus_utility_id, original_plan_id, utility_id, sep = "-") %>%
                    aggregate(n~original_plan_plus_utility_id + date_submitted, data=., sum) -> leads.by.plan_plus_utility_id     
                
                lbp.spread <- spread(leads.by.plan_plus_utility_id,key = original_plan_plus_utility_id, value = n, fill = 0)
                
                lbp.zoo <- zoo(lbp.spread[,-1], lbp.spread[,1])
                
                merge(lbp.zoo, zoo(1, seq(today() - daysback, today(), by = 1)), all = TRUE) %>% 
                    .[,-(ncol(.))] %>%
                    fortify.zoo(.) %>%
                    reshape2::melt("Index") %>%
                    dplyr::rename(date_submitted = Index, original_plan_plus_utility_id = variable, n = value) %>%
                    separate(original_plan_plus_utility_id, into = c("original_plan_id", "utility_id"), sep = "-") -> lbp 
                
                
                
                til.i <- top.impacts.leads[k,]
                ticr.i <- top.impacts.cr[k,]
                
                if(between(til.i$Average_Relative_Effect, ticr.i$Avg_Rel_Eff_Lo_90_Conf_Interval, ticr.i$Avg_Rel_Eff_Up_90_Conf_Interval)){
                    
                    
                    leads.ct[leads.ct$utility_name_id == top.impacts.leads[k, "utility_name_id"],] %>%
                        filter(product == top.impacts.leads[k, "product"]) %>%
                        .[,!names(.) %in% c("utility_name_id", "product")] -> leads.i
                    
                    lbp %>%
                        filter(utility_id == top.impacts.leads[k, "utility_id"]) %>%
                        .[,!names(.) %in% c("utility_id")] %>%
                        spread(key = original_plan_id, value = n, fill = 0) -> lbp.i
                    
                    for.cor.i <- merge(leads.i, lbp.i, by = "date_submitted")
                    
                    arrange(reshape2::melt(cor(for.cor.i$n, for.cor.i[,-1:-2])),desc(value)) %>%
                        .[,!names(.) %in% c("Var1")] %>%
                        dplyr::rename(original_plan_id = Var2, cor = value) -> cor.i
                    
                    lbp.i %>%
                        .[,names(.) %in% c("date_submitted", cor.i$original_plan_id[1:5])] %>%
                        reshape2::melt("date_submitted") %>%
                        dplyr::rename(original_plan_id = variable, n = value) %>%
                        merge(unique(plan.inv[,c("supplier", "plan_name", "original_plan_id")]), by = "original_plan_id", all.x = TRUE) %>%
                        unite(suplan, supplier, plan_name, sep = " - ", remove = TRUE) %>%
                        .[,!names(.) %in% c("original_plan_id")] -> top.lbp
                    
                    changes %>%
                        filter(utility_name_id == top.impacts.leads[k, "utility_name_id"]) %>%
                        filter(original_plan_id == as.numeric(cor.i$original_plan_id[1])) -> changes.i
                    
                    changes.i <- arrange(changes.i[!duplicated(changes.i),], plan_audit_id)
                    
                    leads.i %>%
                        mutate(suplan = "Total Leads") %>%
                        .[,c("date_submitted", "n", "suplan")] %>%
                        rbind(top.lbp) -> leads.for.plot
                    
                    leads.for.plot$suplan <- as.character(leads.for.plot$suplan)
                    leads.for.plot$suplan <- factor(leads.for.plot$suplan, levels = c("Total Leads", unique(top.lbp$suplan)))
                    
                    names(leads.for.plot) <- c("date_submitted", "n", "suplan")
                    
                    ggplot(leads.for.plot, aes(date_submitted, n)) + 
                        geom_line() + 
                        facet_wrap(~suplan, ncol = 1) + 
                        ggtitle("Lead Volume of Top Correlating Plans") +
                        theme(legend.position = "none", panel.background=element_blank(), panel.grid.major = element_line(colour = "grey"), strip.text.y = element_text(angle=0), plot.title = element_text(hjust = 0.5, size = 18))+
                        xlab("Date") +
                        ylab("Lead Volume") +
                        geom_vline(xintercept = as.numeric(change.date.i), linetype = "longdash")
                    
                } else{
                    sessions.ct %>%
                        filter(utility_name_id == top.impacts.leads[k, "utility_name_id"]) %>%
                        aggregate(uniquePageviews ~ date_submitted, data=., sum) -> sessions.for.plot
                    
                    ggplot(sessions.for.plot, aes(date_submitted, uniquePageviews)) + 
                        geom_line() + 
                        ggtitle(paste("Sessions in ", top.impacts.leads[k, "utility"], sep = "")) +
                        theme(legend.position = "none", panel.background=element_blank(), panel.grid.major = element_line(colour = "grey")) +
                        xlab("Date") +
                        ylab("Sessions") +
                        geom_vline(xintercept = as.numeric(change.date.i), linetype = "longdash") +
                        theme(plot.title = element_text(hjust = 0.5, size = 18)) +
                        stat_smooth(method=loess)
                    
                }
                
                
            })
            
            
            # # # --- # # # --- # # # --- # # # --- # # # ---
            
            #                      TAB 3                    #  
            
            # # # --- # # # --- # # # --- # # # --- # # # ---
            
            
            
            
            
            
            
            output$likelihood.description.3 <- renderText({
                
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                leads <- get.causal.impact.data()[[7]]
                
                
                k <- 3
                
                change.date.i <- top.impacts.leads[k, "graph.date"]
                
                leads %>%
                    mutate(n = 1) %>%
                    unite(original_plan_plus_utility_id, original_plan_id, utility_id, sep = "-") %>%
                    aggregate(n~original_plan_plus_utility_id + date_submitted, data=., sum) -> leads.by.plan_plus_utility_id     
                
                lbp.spread <- spread(leads.by.plan_plus_utility_id,key = original_plan_plus_utility_id, value = n, fill = 0)
                
                lbp.zoo <- zoo(lbp.spread[,-1], lbp.spread[,1])
                
                merge(lbp.zoo, zoo(1, seq(today() - daysback, today(), by = 1)), all = TRUE) %>% 
                    .[,-(ncol(.))] %>%
                    fortify.zoo(.) %>%
                    reshape2::melt("Index") %>%
                    dplyr::rename(date_submitted = Index, original_plan_plus_utility_id = variable, n = value) %>%
                    separate(original_plan_plus_utility_id, into = c("original_plan_id", "utility_id"), sep = "-") -> lbp 
                
                
                til.i <- top.impacts.leads[k,]
                ticr.i <- top.impacts.cr[k,]
                
                if(between(til.i$Average_Relative_Effect, ticr.i$Avg_Rel_Eff_Lo_90_Conf_Interval, ticr.i$Avg_Rel_Eff_Up_90_Conf_Interval)){
                    
                    
                    leads.ct[leads.ct$utility_name_id == top.impacts.leads[k, "utility_name_id"],] %>%
                        filter(product == top.impacts.leads[k, "product"]) %>%
                        .[,!names(.) %in% c("utility_name_id", "product")] -> leads.i
                    
                    lbp %>%
                        filter(utility_id == top.impacts.leads[k, "utility_id"]) %>%
                        .[,!names(.) %in% c("utility_id")] %>%
                        spread(key = original_plan_id, value = n, fill = 0) -> lbp.i
                    
                    for.cor.i <- merge(leads.i, lbp.i, by = "date_submitted")
                    
                    arrange(reshape2::melt(cor(for.cor.i$n, for.cor.i[,-1:-2])),desc(value)) %>%
                        .[,!names(.) %in% c("Var1")] %>%
                        dplyr::rename(original_plan_id = Var2, cor = value) -> cor.i
                    
                    lbp.i %>%
                        .[,names(.) %in% c("date_submitted", cor.i$original_plan_id[1:5])] %>%
                        reshape2::melt("date_submitted") %>%
                        dplyr::rename(original_plan_id = variable, n = value) -> top.lbp
                    
                    changes %>%
                        filter(utility_name_id == top.impacts.leads[k, "utility_name_id"]) %>%
                        filter(original_plan_id == as.numeric(cor.i$original_plan_id[1])) -> changes.i
                    
                    changes.i <- arrange(changes.i[!duplicated(changes.i),], plan_audit_id)
                    
                    if(dim(changes.i)[1] == 0){
                        
                        description <- paste0("Both lead volume and conversion rate were down, although no particular plan changes occurred that could be traced back to. See Lead Volume of Top Correlating Plans below for possibile causes.")
                        
                    } else {
                        if(between(changes.i[1,"date"], change.date.i - 3, change.date.i + 3)){
                            
                            description <- paste0("This plan change is highly likely to have caused the observed change in lead and conversion rate.")
                            
                        } else {
                            description <- paste0("Both lead volume and conversion rate were down, although no particular plan changes occurred that could be traced back to. See Lead Volume of Top Correlating Plans below for possibile causes.")
                        }
                    }
                    
                } else{
                    description <- paste0("No plan changes were found relating to this impact, and conversion rate either stayed relatively the same or went up while leads went down. Therefore the impact can be traced back to a change in traffic in this utility zone.")
                }
                
                description
                
            })
            
            
            
            
            
            output$plan_name3.tab <- renderText({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                leads <- get.causal.impact.data()[[7]]
                
                
                k <- 3
                
                change.date.i <- top.impacts.leads[k, "graph.date"]
                
                leads %>%
                    mutate(n = 1) %>%
                    unite(original_plan_plus_utility_id, original_plan_id, utility_id, sep = "-") %>%
                    aggregate(n~original_plan_plus_utility_id + date_submitted, data=., sum) -> leads.by.plan_plus_utility_id     
                
                lbp.spread <- spread(leads.by.plan_plus_utility_id,key = original_plan_plus_utility_id, value = n, fill = 0)
                
                lbp.zoo <- zoo(lbp.spread[,-1], lbp.spread[,1])
                
                merge(lbp.zoo, zoo(1, seq(today() - daysback, today(), by = 1)), all = TRUE) %>% 
                    .[,-(ncol(.))] %>%
                    fortify.zoo(.) %>%
                    reshape2::melt("Index") %>%
                    dplyr::rename(date_submitted = Index, original_plan_plus_utility_id = variable, n = value) %>%
                    separate(original_plan_plus_utility_id, into = c("original_plan_id", "utility_id"), sep = "-") -> lbp 
                
                
                
                
                
                til.i <- top.impacts.leads[k,]
                ticr.i <- top.impacts.cr[k,]
                
                if(between(til.i$Average_Relative_Effect, ticr.i$Avg_Rel_Eff_Lo_90_Conf_Interval, ticr.i$Avg_Rel_Eff_Up_90_Conf_Interval)){
                    
                    
                    leads.ct[leads.ct$utility_name_id == top.impacts.leads[k, "utility_name_id"],] %>%
                        filter(product == top.impacts.leads[k, "product"]) %>%
                        .[,!names(.) %in% c("utility_name_id", "product")] -> leads.i
                    
                    lbp %>%
                        filter(utility_id == top.impacts.leads[k, "utility_id"]) %>%
                        .[,!names(.) %in% c("utility_id")] %>%
                        spread(key = original_plan_id, value = n, fill = 0) -> lbp.i
                    
                    for.cor.i <- merge(leads.i, lbp.i, by = "date_submitted")
                    
                    arrange(reshape2::melt(cor(for.cor.i$n, for.cor.i[,-1:-2])),desc(value)) %>%
                        .[,!names(.) %in% c("Var1")] %>%
                        dplyr::rename(original_plan_id = Var2, cor = value) -> cor.i
                    
                    lbp.i %>%
                        .[,names(.) %in% c("date_submitted", cor.i$original_plan_id[1:5])] %>%
                        reshape2::melt("date_submitted") %>%
                        dplyr::rename(original_plan_id = variable, n = value) -> top.lbp
                    
                    changes %>%
                        filter(utility_name_id == top.impacts.leads[k, "utility_name_id"]) %>%
                        filter(original_plan_id == as.numeric(cor.i$original_plan_id[1])) -> changes.i
                    
                    changes.i <- arrange(changes.i[!duplicated(changes.i),], plan_audit_id)
                    
                    if(dim(changes.i)[1] == 0){
                        
                        paste0("Multiple Possibilities")
                        
                    } else {
                        if(between(changes.i[1,"date"], change.date.i - 3, change.date.i + 3)){
                            
                            as.character(changes.i[1, c("plan_name")])
                            
                        } else {
                            paste0("Multiple Possibilities")
                        }
                        
                    }
                    
                } else{
                    paste0("Traffic Change")
                }
                
                
                
            })
            
            
            output$plan_term3.tab <- renderText({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                leads <- get.causal.impact.data()[[7]]
                
                
                k <- 3
                
                change.date.i <- top.impacts.leads[k, "graph.date"]
                
                leads %>%
                    mutate(n = 1) %>%
                    unite(original_plan_plus_utility_id, original_plan_id, utility_id, sep = "-") %>%
                    aggregate(n~original_plan_plus_utility_id + date_submitted, data=., sum) -> leads.by.plan_plus_utility_id     
                
                lbp.spread <- spread(leads.by.plan_plus_utility_id,key = original_plan_plus_utility_id, value = n, fill = 0)
                
                lbp.zoo <- zoo(lbp.spread[,-1], lbp.spread[,1])
                
                merge(lbp.zoo, zoo(1, seq(today() - daysback, today(), by = 1)), all = TRUE) %>% 
                    .[,-(ncol(.))] %>%
                    fortify.zoo(.) %>%
                    reshape2::melt("Index") %>%
                    dplyr::rename(date_submitted = Index, original_plan_plus_utility_id = variable, n = value) %>%
                    separate(original_plan_plus_utility_id, into = c("original_plan_id", "utility_id"), sep = "-") -> lbp 
                
                
                
                til.i <- top.impacts.leads[k,]
                ticr.i <- top.impacts.cr[k,]
                
                if(between(til.i$Average_Relative_Effect, ticr.i$Avg_Rel_Eff_Lo_90_Conf_Interval, ticr.i$Avg_Rel_Eff_Up_90_Conf_Interval)){
                    
                    
                    leads.ct[leads.ct$utility_name_id == top.impacts.leads[k, "utility_name_id"],] %>%
                        filter(product == top.impacts.leads[k, "product"]) %>%
                        .[,!names(.) %in% c("utility_name_id", "product")] -> leads.i
                    
                    lbp %>%
                        filter(utility_id == top.impacts.leads[k, "utility_id"]) %>%
                        .[,!names(.) %in% c("utility_id")] %>%
                        spread(key = original_plan_id, value = n, fill = 0) -> lbp.i
                    
                    for.cor.i <- merge(leads.i, lbp.i, by = "date_submitted")
                    
                    arrange(reshape2::melt(cor(for.cor.i$n, for.cor.i[,-1:-2])),desc(value)) %>%
                        .[,!names(.) %in% c("Var1")] %>%
                        dplyr::rename(original_plan_id = Var2, cor = value) -> cor.i
                    
                    lbp.i %>%
                        .[,names(.) %in% c("date_submitted", cor.i$original_plan_id[1:5])] %>%
                        reshape2::melt("date_submitted") %>%
                        dplyr::rename(original_plan_id = variable, n = value) -> top.lbp
                    
                    changes %>%
                        filter(utility_name_id == top.impacts.leads[k, "utility_name_id"]) %>%
                        filter(original_plan_id == as.numeric(cor.i$original_plan_id[1])) -> changes.i
                    
                    changes.i <- arrange(changes.i[!duplicated(changes.i),], plan_audit_id)
                    
                    if(dim(changes.i)[1] == 0){
                        
                        paste0(" ")
                        
                    } else {
                        if(between(changes.i[1,"date"], change.date.i - 3, change.date.i + 3)){
                            
                            as.character(paste(changes.i[1, c("contract_term")], " Months ", changes.i[1, c("contract_type")], sep = ""))
                            
                        } else {
                            paste0(" ")
                        }
                        
                    }
                    
                } else{
                    paste0(" ")
                }
                
            }) 
            
            
            output$plan_supplier3.tab <- renderText({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                leads <- get.causal.impact.data()[[7]]
                
                
                k <- 3
                
                change.date.i <- top.impacts.leads[k, "graph.date"]
                
                leads %>%
                    mutate(n = 1) %>%
                    unite(original_plan_plus_utility_id, original_plan_id, utility_id, sep = "-") %>%
                    aggregate(n~original_plan_plus_utility_id + date_submitted, data=., sum) -> leads.by.plan_plus_utility_id     
                
                lbp.spread <- spread(leads.by.plan_plus_utility_id,key = original_plan_plus_utility_id, value = n, fill = 0)
                
                lbp.zoo <- zoo(lbp.spread[,-1], lbp.spread[,1])
                
                merge(lbp.zoo, zoo(1, seq(today() - daysback, today(), by = 1)), all = TRUE) %>% 
                    .[,-(ncol(.))] %>%
                    fortify.zoo(.) %>%
                    reshape2::melt("Index") %>%
                    dplyr::rename(date_submitted = Index, original_plan_plus_utility_id = variable, n = value) %>%
                    separate(original_plan_plus_utility_id, into = c("original_plan_id", "utility_id"), sep = "-") -> lbp 
                
                
                
                til.i <- top.impacts.leads[k,]
                ticr.i <- top.impacts.cr[k,]
                
                if(between(til.i$Average_Relative_Effect, ticr.i$Avg_Rel_Eff_Lo_90_Conf_Interval, ticr.i$Avg_Rel_Eff_Up_90_Conf_Interval)){
                    
                    
                    leads.ct[leads.ct$utility_name_id == top.impacts.leads[k, "utility_name_id"],] %>%
                        filter(product == top.impacts.leads[k, "product"]) %>%
                        .[,!names(.) %in% c("utility_name_id", "product")] -> leads.i
                    
                    lbp %>%
                        filter(utility_id == top.impacts.leads[k, "utility_id"]) %>%
                        .[,!names(.) %in% c("utility_id")] %>%
                        spread(key = original_plan_id, value = n, fill = 0) -> lbp.i
                    
                    for.cor.i <- merge(leads.i, lbp.i, by = "date_submitted")
                    
                    arrange(reshape2::melt(cor(for.cor.i$n, for.cor.i[,-1:-2])),desc(value)) %>%
                        .[,!names(.) %in% c("Var1")] %>%
                        dplyr::rename(original_plan_id = Var2, cor = value) -> cor.i
                    
                    lbp.i %>%
                        .[,names(.) %in% c("date_submitted", cor.i$original_plan_id[1:5])] %>%
                        reshape2::melt("date_submitted") %>%
                        dplyr::rename(original_plan_id = variable, n = value) -> top.lbp
                    
                    changes %>%
                        filter(utility_name_id == top.impacts.leads[k, "utility_name_id"]) %>%
                        filter(original_plan_id == as.numeric(cor.i$original_plan_id[1])) -> changes.i
                    
                    changes.i <- arrange(changes.i[!duplicated(changes.i),], plan_audit_id)
                    
                    if(dim(changes.i)[1] == 0){
                        
                        paste0(" ")
                        
                    } else {
                        if(between(changes.i[1,"date"], change.date.i - 3, change.date.i + 3)){
                            
                            as.character(changes.i[1, c("supplier")])
                            
                        } else {
                            paste0(" ")
                        }
                        
                    }
                    
                } else{
                    paste0(" ")
                }
                
            })
            
            
            output$plan_active3.tab <- renderText({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                leads <- get.causal.impact.data()[[7]]
                
                
                k <- 3
                
                change.date.i <- top.impacts.leads[k, "graph.date"]
                
                leads %>%
                    mutate(n = 1) %>%
                    unite(original_plan_plus_utility_id, original_plan_id, utility_id, sep = "-") %>%
                    aggregate(n~original_plan_plus_utility_id + date_submitted, data=., sum) -> leads.by.plan_plus_utility_id     
                
                lbp.spread <- spread(leads.by.plan_plus_utility_id,key = original_plan_plus_utility_id, value = n, fill = 0)
                
                lbp.zoo <- zoo(lbp.spread[,-1], lbp.spread[,1])
                
                merge(lbp.zoo, zoo(1, seq(today() - daysback, today(), by = 1)), all = TRUE) %>% 
                    .[,-(ncol(.))] %>%
                    fortify.zoo(.) %>%
                    reshape2::melt("Index") %>%
                    dplyr::rename(date_submitted = Index, original_plan_plus_utility_id = variable, n = value) %>%
                    separate(original_plan_plus_utility_id, into = c("original_plan_id", "utility_id"), sep = "-") -> lbp 
                
                
                
                til.i <- top.impacts.leads[k,]
                ticr.i <- top.impacts.cr[k,]
                
                if(between(til.i$Average_Relative_Effect, ticr.i$Avg_Rel_Eff_Lo_90_Conf_Interval, ticr.i$Avg_Rel_Eff_Up_90_Conf_Interval)){
                    
                    
                    leads.ct[leads.ct$utility_name_id == top.impacts.leads[k, "utility_name_id"],] %>%
                        filter(product == top.impacts.leads[k, "product"]) %>%
                        .[,!names(.) %in% c("utility_name_id", "product")] -> leads.i
                    
                    lbp %>%
                        filter(utility_id == top.impacts.leads[k, "utility_id"]) %>%
                        .[,!names(.) %in% c("utility_id")] %>%
                        spread(key = original_plan_id, value = n, fill = 0) -> lbp.i
                    
                    for.cor.i <- merge(leads.i, lbp.i, by = "date_submitted")
                    
                    arrange(reshape2::melt(cor(for.cor.i$n, for.cor.i[,-1:-2])),desc(value)) %>%
                        .[,!names(.) %in% c("Var1")] %>%
                        dplyr::rename(original_plan_id = Var2, cor = value) -> cor.i
                    
                    lbp.i %>%
                        .[,names(.) %in% c("date_submitted", cor.i$original_plan_id[1:5])] %>%
                        reshape2::melt("date_submitted") %>%
                        dplyr::rename(original_plan_id = variable, n = value) -> top.lbp
                    
                    changes %>%
                        filter(utility_name_id == top.impacts.leads[k, "utility_name_id"]) %>%
                        filter(original_plan_id == as.numeric(cor.i$original_plan_id[1])) -> changes.i
                    
                    changes.i <- arrange(changes.i[!duplicated(changes.i),], plan_audit_id)
                    
                    if(dim(changes.i)[1] == 0){
                        
                        paste0(" ")
                        
                    } else {
                        if(between(changes.i[1,"date"], change.date.i - 3, change.date.i + 3)){
                            
                            new_plan_status = changes.i[changes.i$plan_audit_id == max(changes.i$plan_audit_id), "plan_status_name"]
                            
                            new_plan_status
                            
                        } else {
                            paste0(" ")
                        }
                        
                    }
                    
                } else{
                    paste0(" ")
                }
                
            })
            
            output$plan_id3.tab <- renderText({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                leads <- get.causal.impact.data()[[7]]
                
                
                k <- 3
                
                change.date.i <- top.impacts.leads[k, "graph.date"]
                
                leads %>%
                    mutate(n = 1) %>%
                    unite(original_plan_plus_utility_id, original_plan_id, utility_id, sep = "-") %>%
                    aggregate(n~original_plan_plus_utility_id + date_submitted, data=., sum) -> leads.by.plan_plus_utility_id     
                
                lbp.spread <- spread(leads.by.plan_plus_utility_id,key = original_plan_plus_utility_id, value = n, fill = 0)
                
                lbp.zoo <- zoo(lbp.spread[,-1], lbp.spread[,1])
                
                merge(lbp.zoo, zoo(1, seq(today() - daysback, today(), by = 1)), all = TRUE) %>% 
                    .[,-(ncol(.))] %>%
                    fortify.zoo(.) %>%
                    reshape2::melt("Index") %>%
                    dplyr::rename(date_submitted = Index, original_plan_plus_utility_id = variable, n = value) %>%
                    separate(original_plan_plus_utility_id, into = c("original_plan_id", "utility_id"), sep = "-") -> lbp 
                
                
                
                til.i <- top.impacts.leads[k,]
                ticr.i <- top.impacts.cr[k,]
                
                if(between(til.i$Average_Relative_Effect, ticr.i$Avg_Rel_Eff_Lo_90_Conf_Interval, ticr.i$Avg_Rel_Eff_Up_90_Conf_Interval)){
                    
                    
                    leads.ct[leads.ct$utility_name_id == top.impacts.leads[k, "utility_name_id"],] %>%
                        filter(product == top.impacts.leads[k, "product"]) %>%
                        .[,!names(.) %in% c("utility_name_id", "product")] -> leads.i
                    
                    lbp %>%
                        filter(utility_id == top.impacts.leads[k, "utility_id"]) %>%
                        .[,!names(.) %in% c("utility_id")] %>%
                        spread(key = original_plan_id, value = n, fill = 0) -> lbp.i
                    
                    for.cor.i <- merge(leads.i, lbp.i, by = "date_submitted")
                    
                    arrange(reshape2::melt(cor(for.cor.i$n, for.cor.i[,-1:-2])),desc(value)) %>%
                        .[,!names(.) %in% c("Var1")] %>%
                        dplyr::rename(original_plan_id = Var2, cor = value) -> cor.i
                    
                    lbp.i %>%
                        .[,names(.) %in% c("date_submitted", cor.i$original_plan_id[1:5])] %>%
                        reshape2::melt("date_submitted") %>%
                        dplyr::rename(original_plan_id = variable, n = value) -> top.lbp
                    
                    changes %>%
                        filter(utility_name_id == top.impacts.leads[k, "utility_name_id"]) %>%
                        filter(original_plan_id == as.numeric(cor.i$original_plan_id[1])) -> changes.i
                    
                    changes.i <- arrange(changes.i[!duplicated(changes.i),], plan_audit_id)
                    
                    
                    if(dim(changes.i)[1] == 0){
                        
                        paste0(" ")
                        
                    } else {
                        if(between(changes.i[1,"date"], change.date.i - 3, change.date.i + 3)){
                            
                            as.character(paste("Plan ID: ", changes.i[1, c("original_plan_id")], sep = ""))
                            
                        } else {
                            paste0(" ")
                        }
                        
                    }
                    
                } else{
                    paste0(" ")
                }
                
                
            })
            
            
            
            output$price_change3 <- renderText({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                leads <- get.causal.impact.data()[[7]]
                
                
                k <- 3
                
                change.date.i <- top.impacts.leads[k, "graph.date"]
                
                leads %>%
                    mutate(n = 1) %>%
                    unite(original_plan_plus_utility_id, original_plan_id, utility_id, sep = "-") %>%
                    aggregate(n~original_plan_plus_utility_id + date_submitted, data=., sum) -> leads.by.plan_plus_utility_id     
                
                lbp.spread <- spread(leads.by.plan_plus_utility_id,key = original_plan_plus_utility_id, value = n, fill = 0)
                
                lbp.zoo <- zoo(lbp.spread[,-1], lbp.spread[,1])
                
                merge(lbp.zoo, zoo(1, seq(today() - daysback, today(), by = 1)), all = TRUE) %>% 
                    .[,-(ncol(.))] %>%
                    fortify.zoo(.) %>%
                    reshape2::melt("Index") %>%
                    dplyr::rename(date_submitted = Index, original_plan_plus_utility_id = variable, n = value) %>%
                    separate(original_plan_plus_utility_id, into = c("original_plan_id", "utility_id"), sep = "-") -> lbp 
                
                
                
                til.i <- top.impacts.leads[k,]
                ticr.i <- top.impacts.cr[k,]
                
                if(between(til.i$Average_Relative_Effect, ticr.i$Avg_Rel_Eff_Lo_90_Conf_Interval, ticr.i$Avg_Rel_Eff_Up_90_Conf_Interval)){
                    
                    
                    leads.ct[leads.ct$utility_name_id == top.impacts.leads[k, "utility_name_id"],] %>%
                        filter(product == top.impacts.leads[k, "product"]) %>%
                        .[,!names(.) %in% c("utility_name_id", "product")] -> leads.i
                    
                    lbp %>%
                        filter(utility_id == top.impacts.leads[k, "utility_id"]) %>%
                        .[,!names(.) %in% c("utility_id")] %>%
                        spread(key = original_plan_id, value = n, fill = 0) -> lbp.i
                    
                    for.cor.i <- merge(leads.i, lbp.i, by = "date_submitted")
                    
                    arrange(reshape2::melt(cor(for.cor.i$n, for.cor.i[,-1:-2])),desc(value)) %>%
                        .[,!names(.) %in% c("Var1")] %>%
                        dplyr::rename(original_plan_id = Var2, cor = value) -> cor.i
                    
                    lbp.i %>%
                        .[,names(.) %in% c("date_submitted", cor.i$original_plan_id[1:5])] %>%
                        reshape2::melt("date_submitted") %>%
                        dplyr::rename(original_plan_id = variable, n = value) -> top.lbp
                    
                    changes %>%
                        filter(utility_name_id == top.impacts.leads[k, "utility_name_id"]) %>%
                        filter(original_plan_id == as.numeric(cor.i$original_plan_id[1])) -> changes.i
                    
                    changes.i <- arrange(changes.i[!duplicated(changes.i),], plan_audit_id)
                    
                    
                    
                    if(dim(changes.i)[1] == 0){
                        
                        paste0(" ")
                        
                    } else {
                        if(between(changes.i[1,"date"], change.date.i - 3, change.date.i + 3)){
                            
                            old_price = changes.i[changes.i$plan_audit_id == min(changes.i$plan_audit_id), "plan_price"]
                            new_price = changes.i[changes.i$plan_audit_id == max(changes.i$plan_audit_id), "plan_price"]
                            
                            paste(old_price, "¢", " -> ", new_price, "¢", sep = "")
                            
                        } else {
                            paste0(" ")
                        }
                        
                    }
                    
                } else{
                    paste0(" ")
                }
                
                
            })
            
            
            
            
            
            
            output$change.3 <- renderInfoBox({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                leads <- get.causal.impact.data()[[7]]
                
                k <- 3
                
                change.date.i <- max(changes[changes$change_index == top.impacts.leads$change_index[k], "date"])
                
                product.i <- unique(changes[changes$change_index == top.impacts.leads$change_index[k], "product"])
                utility.i <- paste(unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility"]), unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility_id"]), sep = " - ")
                
                leads.ct %>%
                    filter(utility_name_id == utility.i) %>%
                    filter(product == product.i) -> leads.i
                
                leads.ts <- zoo(leads.i[,!names(leads.i) %in% c("date_submitted", "utility_name_id", "product")], order.by = leads.i$date_submitted)
                pre.i <- c(min(leads.i$date_submitted), change.date.i)
                post.i <- c(change.date.i + 1, max(leads.i$date_submitted))
                impact.i <- try.Causal.Impact(time_series = leads.ts, pre_date = pre.i, post_date = post.i, conf_int = 0.05)
                
                if(impact.i$summary$AbsEffect[1] < 0){
                    subt <- "Lead Volume Decline"
                    ico <- icon("arrow-down")
                } else {
                    subt <- "Lead Volume Increase"
                    ico <- icon("arrow-up")
                }
               
                
                infoBox(
                    "What occurred:", subt, icon = ico,
                    color = "purple"
                )
                
            })
            
            output$utility.3 <- renderInfoBox({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                
                k <- 3
                infoBox(
                    "Where:", paste(top.impacts.leads[k, c("state")], top.impacts.leads[k, c("utility")], sep = " - "), icon = icon("plug", "fa-0.5x"),
                    color = "green"
                )        
            })
            
            output$product.3 <- renderInfoBox({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                
                k <- 3
                if(top.impacts.leads[k, c("product")] == "Electricity"){
                    ico <- icon("bolt", "fa-0.5x")
                } else{
                    ico <- icon("fire", "fa-0.5x")
                }
                infoBox(
                    "Which product:", top.impacts.leads[k, c("product")], icon = ico,
                    color = "yellow"
                )        
            })
            
            
            output$causal.impact.cr.plot3 <- renderPlot({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                
                changes.unique <- changes[!duplicated(changes[,c("date", "utility_id")]),]
                unique.index <- unique(changes.unique$change_index)
                
                k <- 3
                
                change.date.i <- max(changes[changes$change_index == top.impacts.leads$change_index[k], "date"])
                
                product.i <- unique(changes[changes$change_index == top.impacts.leads$change_index[k], "product"])
                utility.i <- paste(unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility"]), unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility_id"]), sep = " - ")
                
                cr.ct %>%
                    filter(utility_name_id == utility.i) %>%
                    filter(product == product.i) -> leads.i
                
                leads.ts <- zoo(leads.i[,!names(leads.i) %in% c("date_submitted", "utility_name_id", "product")], order.by = leads.i$date_submitted)
                pre.i <- c(min(leads.i$date_submitted), change.date.i)
                post.i <- c(change.date.i + 1, max(leads.i$date_submitted))
                impact.i <- try.Causal.Impact(time_series = leads.ts, pre_date = pre.i, post_date = post.i, conf_int = 0.05)
                plot(impact.i)
            })
            
            
            output$causal.impact.cr.summary.EXCLUDE <- renderPrint({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                
                changes.unique <- changes[!duplicated(changes[,c("date", "utility_id")]),]
                unique.index <- unique(changes.unique$change_index)
                
                k <- 3
                
                change.date.i <- max(changes[changes$change_index == top.impacts.leads$change_index[k], "date"])
                
                product.i <- unique(changes[changes$change_index == top.impacts.leads$change_index[k], "product"])
                utility.i <- paste(unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility"]), unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility_id"]), sep = " - ")
                
                cr.ct %>%
                    filter(utility_name_id == utility.i) %>%
                    filter(product == product.i) -> leads.i
                
                leads.ts <- zoo(leads.i[,!names(leads.i) %in% c("date_submitted", "utility_name_id", "product")], order.by = leads.i$date_submitted)
                pre.i <- c(min(leads.i$date_submitted), change.date.i)
                post.i <- c(change.date.i + 1, max(leads.i$date_submitted))
                impact.i <- try.Causal.Impact(time_series = leads.ts, pre_date = pre.i, post_date = post.i, conf_int = 0.05)
                summary(impact.i)
            })        
            
            
            output$causal.impact.cr.summary3 <- renderPrint({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                
                changes.unique <- changes[!duplicated(changes[,c("date", "utility_id")]),]
                unique.index <- unique(changes.unique$change_index)
                
                k <- 3
                
                change.date.i <- max(changes[changes$change_index == top.impacts.leads$change_index[k], "date"])
                
                product.i <- unique(changes[changes$change_index == top.impacts.leads$change_index[k], "product"])
                utility.i <- paste(unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility"]), unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility_id"]), sep = " - ")
                cr.ct %>%
                    filter(utility_name_id == utility.i) %>%
                    filter(product == product.i) -> leads.i
                
                leads.ts <- zoo(leads.i[,!names(leads.i) %in% c("date_submitted", "utility_name_id", "product")], order.by = leads.i$date_submitted)
                pre.i <- c(min(leads.i$date_submitted), change.date.i)
                post.i <- c(change.date.i + 1, max(leads.i$date_submitted))
                impact.i <- try.Causal.Impact(time_series = leads.ts, pre_date = pre.i, post_date = post.i, conf_int = 0.05)
                summary(impact.i)
            })
            
            output$causal.impact.leads.summary3 <- renderPrint({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                
                changes.unique <- changes[!duplicated(changes[,c("date", "utility_id")]),]
                unique.index <- unique(changes.unique$change_index)
                
                k <- 3
                
                change.date.i <- max(changes[changes$change_index == top.impacts.leads$change_index[k], "date"])
                
                product.i <- unique(changes[changes$change_index == top.impacts.leads$change_index[k], "product"])
                utility.i <- paste(unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility"]), unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility_id"]), sep = " - ")
                leads.ct %>%
                    filter(utility_name_id == utility.i) %>%
                    filter(product == product.i) -> leads.i
                
                leads.ts <- zoo(leads.i[,!names(leads.i) %in% c("date_submitted", "utility_name_id", "product")], order.by = leads.i$date_submitted)
                pre.i <- c(min(leads.i$date_submitted), change.date.i)
                post.i <- c(change.date.i + 1, max(leads.i$date_submitted))
                impact.i <- try.Causal.Impact(time_series = leads.ts, pre_date = pre.i, post_date = post.i, conf_int = 0.05)
                summary(impact.i)
            })
            
            
            output$causal.impact.leads.plot3 <- renderPlot({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                
                changes.unique <- changes[!duplicated(changes[,c("date", "utility_id")]),]
                unique.index <- unique(changes.unique$change_index)
                
                k <- 3
                
                change.date.i <- max(changes[changes$change_index == top.impacts.leads$change_index[k], "date"])
                
                product.i <- unique(changes[changes$change_index == top.impacts.leads$change_index[k], "product"])
                utility.i <- paste(unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility"]), unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility_id"]), sep = " - ")
                leads.ct %>%
                    filter(utility_name_id == utility.i) %>%
                    filter(product == product.i) -> leads.i
                
                leads.ts <- zoo(leads.i[,!names(leads.i) %in% c("date_submitted", "utility_name_id", "product")], order.by = leads.i$date_submitted)
                pre.i <- c(min(leads.i$date_submitted), change.date.i)
                post.i <- c(change.date.i + 1, max(leads.i$date_submitted))
                impact.i <- try.Causal.Impact(time_series = leads.ts, pre_date = pre.i, post_date = post.i, conf_int = 0.05)
                plot(impact.i)
            })
            
            
            output$complete.plan.changes3 <- renderPlotly({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                
                changes.unique <- changes[!duplicated(changes[,c("date", "utility_id")]),]
                unique.index <- unique(changes.unique$change_index)
                
                k <- 3
                
                change.date.i <- max(changes[changes$change_index == top.impacts.leads$change_index[k], "date"])
                
                product.i <- unique(changes[changes$change_index == top.impacts.leads$change_index[k], "product"])
                utility.i <- paste(unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility"]), unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility_id"]), sep = " - ")
                
                utility.name.i <- unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility_name_id"])
                changes[changes$utility_name_id == utility.name.i, ] %>%
                    filter(graph.date > (today()-(daysback+1))) %>%
                    mutate(Supplier_Plan = paste(change, supplier, plan_name, sep = " - ")) -> changes.full.i
                
                daily.prices %>%
                    filter(utility_name_id == utility.i) %>%
                    filter(product == product.i) -> daily.prices.i
                
                
                
                p <- ggplot() + 
                    geom_line(data = daily.prices.i, aes(date, plan_price.mean)) +
                    geom_line(data = daily.prices.i, aes(date, plan_price.max)) +
                    geom_line(data = daily.prices.i, aes(date, plan_price.min)) +
                    geom_line(data = changes.full.i[changes.full.i$change == "plan_change",], aes(graph.date, plan_price, col = Supplier_Plan)) +
                    geom_point(data = changes.full.i[changes.full.i$change == "plan_end",], aes(graph.date, plan_price, col = Supplier_Plan)) + 
                    geom_point(data = changes.full.i[changes.full.i$change == "new_plan",], aes(graph.date, plan_price, col = Supplier_Plan)) +
                    geom_vline(xintercept = as.numeric(change.date.i), linetype = "longdash") +
                    ggtitle(paste("All Plan Changes Around", change.date.i, sep = " ")) +
                    xlab("Date") +
                    ylab("Plan Price (cents)") +
                    theme(legend.position = "none", panel.background=element_blank(), panel.grid.major = element_line(colour = "grey"))
                
                ggplotly(p)
            })
            
            
            
            
            
            output$plan_breakdown3 <- renderPlot({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                leads <- get.causal.impact.data()[[7]]
                sessions.ct <- get.causal.impact.data()[[8]]     
                plan.inv <- get.causal.impact.data()[[9]]
                
                
                changes.unique <- changes[!duplicated(changes[,c("date", "utility_id")]),]
                unique.index <- unique(changes.unique$change_index)
                
                k <- 3
                
                change.date.i <- max(changes[changes$change_index == top.impacts.leads$change_index[k], "date"])
                
                product.i <- unique(changes[changes$change_index == top.impacts.leads$change_index[k], "product"])
                utility.i <- paste(unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility"]), unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility_id"]), sep = " - ")
                
                utility.name.i <- unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility_name_id"])
                changes[changes$utility_name_id == utility.name.i, ] %>%
                    filter(graph.date > (today()-(daysback+1))) %>%
                    mutate(suplan = paste(change, supplier, plan_name, sep = " - ")) -> changes.full.i
                
                daily.prices %>%
                    filter(utility_name_id == utility.i) %>%
                    filter(product == product.i) -> daily.prices.i
                
                
                leads %>%
                    mutate(n = 1) %>%
                    unite(original_plan_plus_utility_id, original_plan_id, utility_id, sep = "-") %>%
                    aggregate(n~original_plan_plus_utility_id + date_submitted, data=., sum) -> leads.by.plan_plus_utility_id     
                
                lbp.spread <- spread(leads.by.plan_plus_utility_id,key = original_plan_plus_utility_id, value = n, fill = 0)
                
                lbp.zoo <- zoo(lbp.spread[,-1], lbp.spread[,1])
                
                merge(lbp.zoo, zoo(1, seq(today() - daysback, today(), by = 1)), all = TRUE) %>% 
                    .[,-(ncol(.))] %>%
                    fortify.zoo(.) %>%
                    reshape2::melt("Index") %>%
                    dplyr::rename(date_submitted = Index, original_plan_plus_utility_id = variable, n = value) %>%
                    separate(original_plan_plus_utility_id, into = c("original_plan_id", "utility_id"), sep = "-") -> lbp 
                
                
                
                til.i <- top.impacts.leads[k,]
                ticr.i <- top.impacts.cr[k,]
                
                if(between(til.i$Average_Relative_Effect, ticr.i$Avg_Rel_Eff_Lo_90_Conf_Interval, ticr.i$Avg_Rel_Eff_Up_90_Conf_Interval)){
                    
                    
                    leads.ct[leads.ct$utility_name_id == top.impacts.leads[k, "utility_name_id"],] %>%
                        filter(product == top.impacts.leads[k, "product"]) %>%
                        .[,!names(.) %in% c("utility_name_id", "product")] -> leads.i
                    
                    lbp %>%
                        filter(utility_id == top.impacts.leads[k, "utility_id"]) %>%
                        .[,!names(.) %in% c("utility_id")] %>%
                        spread(key = original_plan_id, value = n, fill = 0) -> lbp.i
                    
                    for.cor.i <- merge(leads.i, lbp.i, by = "date_submitted")
                    
                    arrange(reshape2::melt(cor(for.cor.i$n, for.cor.i[,-1:-2])),desc(value)) %>%
                        .[,!names(.) %in% c("Var1")] %>%
                        dplyr::rename(original_plan_id = Var2, cor = value) -> cor.i
                    
                    lbp.i %>%
                        .[,names(.) %in% c("date_submitted", cor.i$original_plan_id[1:5])] %>%
                        reshape2::melt("date_submitted") %>%
                        dplyr::rename(original_plan_id = variable, n = value) %>%
                        merge(unique(plan.inv[,c("supplier", "plan_name", "original_plan_id")]), by = "original_plan_id", all.x = TRUE) %>%
                        unite(suplan, supplier, plan_name, sep = " - ", remove = TRUE) %>%
                        .[,!names(.) %in% c("original_plan_id")] -> top.lbp
                    
                    changes %>%
                        filter(utility_name_id == top.impacts.leads[k, "utility_name_id"]) %>%
                        filter(original_plan_id == as.numeric(cor.i$original_plan_id[1])) -> changes.i
                    
                    changes.i <- arrange(changes.i[!duplicated(changes.i),], plan_audit_id)
                    
                    leads.i %>%
                        mutate(suplan = "Total Leads") %>%
                        .[,c("date_submitted", "n", "suplan")] %>%
                        rbind(top.lbp) -> leads.for.plot
                    
                    leads.for.plot$suplan <- as.character(leads.for.plot$suplan)
                    leads.for.plot$suplan <- factor(leads.for.plot$suplan, levels = c("Total Leads", unique(top.lbp$suplan)))
                    
                    names(leads.for.plot) <- c("date_submitted", "n", "suplan")
                    
                    ggplot(leads.for.plot, aes(date_submitted, n)) + 
                        geom_line() + 
                        facet_wrap(~suplan, ncol = 1) + 
                        ggtitle("Lead Volume of Top Correlating Plans") +
                        theme(legend.position = "none", panel.background=element_blank(), panel.grid.major = element_line(colour = "grey"), strip.text.y = element_text(angle=0), plot.title = element_text(hjust = 0.5, size = 18))+
                        xlab("Date") +
                        ylab("Lead Volume") +
                        geom_vline(xintercept = as.numeric(change.date.i), linetype = "longdash")
                    
                } else{
                    sessions.ct %>%
                        filter(utility_name_id == top.impacts.leads[k, "utility_name_id"]) %>%
                        aggregate(uniquePageviews ~ date_submitted, data=., sum) -> sessions.for.plot
                    
                    ggplot(sessions.for.plot, aes(date_submitted, uniquePageviews)) + 
                        geom_line() + 
                        ggtitle(paste("Sessions in ", top.impacts.leads[k, "utility"], sep = "")) +
                        theme(legend.position = "none", panel.background=element_blank(), panel.grid.major = element_line(colour = "grey")) +
                        xlab("Date") +
                        ylab("Sessions") +
                        geom_vline(xintercept = as.numeric(change.date.i), linetype = "longdash") +
                        theme(plot.title = element_text(hjust = 0.5, size = 18)) +
                        stat_smooth(method=loess)
                    
                }
                
                
            })
            
            
            # # # --- # # # --- # # # --- # # # --- # # # ---
            
            #                      TAB 4                    #  
            
            # # # --- # # # --- # # # --- # # # --- # # # ---
            
            
            
            
            
            
            
            output$likelihood.description.4 <- renderText({
                
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                leads <- get.causal.impact.data()[[7]]
                
                
                k <- 4
                
                change.date.i <- top.impacts.leads[k, "graph.date"]
                
                leads %>%
                    mutate(n = 1) %>%
                    unite(original_plan_plus_utility_id, original_plan_id, utility_id, sep = "-") %>%
                    aggregate(n~original_plan_plus_utility_id + date_submitted, data=., sum) -> leads.by.plan_plus_utility_id     
                
                lbp.spread <- spread(leads.by.plan_plus_utility_id,key = original_plan_plus_utility_id, value = n, fill = 0)
                
                lbp.zoo <- zoo(lbp.spread[,-1], lbp.spread[,1])
                
                merge(lbp.zoo, zoo(1, seq(today() - daysback, today(), by = 1)), all = TRUE) %>% 
                    .[,-(ncol(.))] %>%
                    fortify.zoo(.) %>%
                    reshape2::melt("Index") %>%
                    dplyr::rename(date_submitted = Index, original_plan_plus_utility_id = variable, n = value) %>%
                    separate(original_plan_plus_utility_id, into = c("original_plan_id", "utility_id"), sep = "-") -> lbp 
                
                
                til.i <- top.impacts.leads[k,]
                ticr.i <- top.impacts.cr[k,]
                
                if(between(til.i$Average_Relative_Effect, ticr.i$Avg_Rel_Eff_Lo_90_Conf_Interval, ticr.i$Avg_Rel_Eff_Up_90_Conf_Interval)){
                    
                    
                    leads.ct[leads.ct$utility_name_id == top.impacts.leads[k, "utility_name_id"],] %>%
                        filter(product == top.impacts.leads[k, "product"]) %>%
                        .[,!names(.) %in% c("utility_name_id", "product")] -> leads.i
                    
                    lbp %>%
                        filter(utility_id == top.impacts.leads[k, "utility_id"]) %>%
                        .[,!names(.) %in% c("utility_id")] %>%
                        spread(key = original_plan_id, value = n, fill = 0) -> lbp.i
                    
                    for.cor.i <- merge(leads.i, lbp.i, by = "date_submitted")
                    
                    arrange(reshape2::melt(cor(for.cor.i$n, for.cor.i[,-1:-2])),desc(value)) %>%
                        .[,!names(.) %in% c("Var1")] %>%
                        dplyr::rename(original_plan_id = Var2, cor = value) -> cor.i
                    
                    lbp.i %>%
                        .[,names(.) %in% c("date_submitted", cor.i$original_plan_id[1:5])] %>%
                        reshape2::melt("date_submitted") %>%
                        dplyr::rename(original_plan_id = variable, n = value) -> top.lbp
                    
                    changes %>%
                        filter(utility_name_id == top.impacts.leads[k, "utility_name_id"]) %>%
                        filter(original_plan_id == as.numeric(cor.i$original_plan_id[1])) -> changes.i
                    
                    changes.i <- arrange(changes.i[!duplicated(changes.i),], plan_audit_id)
                    
                    if(dim(changes.i)[1] == 0){
                        
                        description <- paste0("Both lead volume and conversion rate were down, although no particular plan changes occurred that could be traced back to. See Lead Volume of Top Correlating Plans below for possibile causes.")
                        
                    } else {
                        if(between(changes.i[1,"date"], change.date.i - 3, change.date.i + 3)){
                            
                            description <- paste0("This plan change is highly likely to have caused the observed change in lead and conversion rate.")
                            
                        } else {
                            description <- paste0("Both lead volume and conversion rate were down, although no particular plan changes occurred that could be traced back to. See Lead Volume of Top Correlating Plans below for possibile causes.")
                        }
                    }
                    
                } else{
                    description <- paste0("No plan changes were found relating to this impact, and conversion rate either stayed relatively the same or went up while leads went down. Therefore the impact can be traced back to a change in traffic in this utility zone.")
                }
                
                description
                
            })
            
            
            
            
            
            output$plan_name4.tab <- renderText({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                leads <- get.causal.impact.data()[[7]]
                
                
                k <- 4
                
                change.date.i <- top.impacts.leads[k, "graph.date"]
                
                leads %>%
                    mutate(n = 1) %>%
                    unite(original_plan_plus_utility_id, original_plan_id, utility_id, sep = "-") %>%
                    aggregate(n~original_plan_plus_utility_id + date_submitted, data=., sum) -> leads.by.plan_plus_utility_id     
                
                lbp.spread <- spread(leads.by.plan_plus_utility_id,key = original_plan_plus_utility_id, value = n, fill = 0)
                
                lbp.zoo <- zoo(lbp.spread[,-1], lbp.spread[,1])
                
                merge(lbp.zoo, zoo(1, seq(today() - daysback, today(), by = 1)), all = TRUE) %>% 
                    .[,-(ncol(.))] %>%
                    fortify.zoo(.) %>%
                    reshape2::melt("Index") %>%
                    dplyr::rename(date_submitted = Index, original_plan_plus_utility_id = variable, n = value) %>%
                    separate(original_plan_plus_utility_id, into = c("original_plan_id", "utility_id"), sep = "-") -> lbp 
                
                
                
                
                
                til.i <- top.impacts.leads[k,]
                ticr.i <- top.impacts.cr[k,]
                
                if(between(til.i$Average_Relative_Effect, ticr.i$Avg_Rel_Eff_Lo_90_Conf_Interval, ticr.i$Avg_Rel_Eff_Up_90_Conf_Interval)){
                    
                    
                    leads.ct[leads.ct$utility_name_id == top.impacts.leads[k, "utility_name_id"],] %>%
                        filter(product == top.impacts.leads[k, "product"]) %>%
                        .[,!names(.) %in% c("utility_name_id", "product")] -> leads.i
                    
                    lbp %>%
                        filter(utility_id == top.impacts.leads[k, "utility_id"]) %>%
                        .[,!names(.) %in% c("utility_id")] %>%
                        spread(key = original_plan_id, value = n, fill = 0) -> lbp.i
                    
                    for.cor.i <- merge(leads.i, lbp.i, by = "date_submitted")
                    
                    arrange(reshape2::melt(cor(for.cor.i$n, for.cor.i[,-1:-2])),desc(value)) %>%
                        .[,!names(.) %in% c("Var1")] %>%
                        dplyr::rename(original_plan_id = Var2, cor = value) -> cor.i
                    
                    lbp.i %>%
                        .[,names(.) %in% c("date_submitted", cor.i$original_plan_id[1:5])] %>%
                        reshape2::melt("date_submitted") %>%
                        dplyr::rename(original_plan_id = variable, n = value) -> top.lbp
                    
                    changes %>%
                        filter(utility_name_id == top.impacts.leads[k, "utility_name_id"]) %>%
                        filter(original_plan_id == as.numeric(cor.i$original_plan_id[1])) -> changes.i
                    
                    changes.i <- arrange(changes.i[!duplicated(changes.i),], plan_audit_id)
                    
                    if(dim(changes.i)[1] == 0){
                        
                        paste0("Multiple Possibilities")
                        
                    } else {
                        if(between(changes.i[1,"date"], change.date.i - 3, change.date.i + 3)){
                            
                            as.character(changes.i[1, c("plan_name")])
                            
                        } else {
                            paste0("Multiple Possibilities")
                        }
                        
                    }
                    
                } else{
                    paste0("Traffic Change")
                }
                
                
                
            })
            
            
            output$plan_term4.tab <- renderText({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                leads <- get.causal.impact.data()[[7]]
                
                
                k <- 4
                
                change.date.i <- top.impacts.leads[k, "graph.date"]
                
                leads %>%
                    mutate(n = 1) %>%
                    unite(original_plan_plus_utility_id, original_plan_id, utility_id, sep = "-") %>%
                    aggregate(n~original_plan_plus_utility_id + date_submitted, data=., sum) -> leads.by.plan_plus_utility_id     
                
                lbp.spread <- spread(leads.by.plan_plus_utility_id,key = original_plan_plus_utility_id, value = n, fill = 0)
                
                lbp.zoo <- zoo(lbp.spread[,-1], lbp.spread[,1])
                
                merge(lbp.zoo, zoo(1, seq(today() - daysback, today(), by = 1)), all = TRUE) %>% 
                    .[,-(ncol(.))] %>%
                    fortify.zoo(.) %>%
                    reshape2::melt("Index") %>%
                    dplyr::rename(date_submitted = Index, original_plan_plus_utility_id = variable, n = value) %>%
                    separate(original_plan_plus_utility_id, into = c("original_plan_id", "utility_id"), sep = "-") -> lbp 
                
                
                
                til.i <- top.impacts.leads[k,]
                ticr.i <- top.impacts.cr[k,]
                
                if(between(til.i$Average_Relative_Effect, ticr.i$Avg_Rel_Eff_Lo_90_Conf_Interval, ticr.i$Avg_Rel_Eff_Up_90_Conf_Interval)){
                    
                    
                    leads.ct[leads.ct$utility_name_id == top.impacts.leads[k, "utility_name_id"],] %>%
                        filter(product == top.impacts.leads[k, "product"]) %>%
                        .[,!names(.) %in% c("utility_name_id", "product")] -> leads.i
                    
                    lbp %>%
                        filter(utility_id == top.impacts.leads[k, "utility_id"]) %>%
                        .[,!names(.) %in% c("utility_id")] %>%
                        spread(key = original_plan_id, value = n, fill = 0) -> lbp.i
                    
                    for.cor.i <- merge(leads.i, lbp.i, by = "date_submitted")
                    
                    arrange(reshape2::melt(cor(for.cor.i$n, for.cor.i[,-1:-2])),desc(value)) %>%
                        .[,!names(.) %in% c("Var1")] %>%
                        dplyr::rename(original_plan_id = Var2, cor = value) -> cor.i
                    
                    lbp.i %>%
                        .[,names(.) %in% c("date_submitted", cor.i$original_plan_id[1:5])] %>%
                        reshape2::melt("date_submitted") %>%
                        dplyr::rename(original_plan_id = variable, n = value) -> top.lbp
                    
                    changes %>%
                        filter(utility_name_id == top.impacts.leads[k, "utility_name_id"]) %>%
                        filter(original_plan_id == as.numeric(cor.i$original_plan_id[1])) -> changes.i
                    
                    changes.i <- arrange(changes.i[!duplicated(changes.i),], plan_audit_id)
                    
                    if(dim(changes.i)[1] == 0){
                        
                        paste0(" ")
                        
                    } else {
                        if(between(changes.i[1,"date"], change.date.i - 3, change.date.i + 3)){
                            
                            as.character(paste(changes.i[1, c("contract_term")], " Months ", changes.i[1, c("contract_type")], sep = ""))
                            
                        } else {
                            paste0(" ")
                        }
                        
                    }
                    
                } else{
                    paste0(" ")
                }
                
            }) 
            
            
            output$plan_supplier4.tab <- renderText({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                leads <- get.causal.impact.data()[[7]]
                
                
                k <- 4
                
                change.date.i <- top.impacts.leads[k, "graph.date"]
                
                leads %>%
                    mutate(n = 1) %>%
                    unite(original_plan_plus_utility_id, original_plan_id, utility_id, sep = "-") %>%
                    aggregate(n~original_plan_plus_utility_id + date_submitted, data=., sum) -> leads.by.plan_plus_utility_id     
                
                lbp.spread <- spread(leads.by.plan_plus_utility_id,key = original_plan_plus_utility_id, value = n, fill = 0)
                
                lbp.zoo <- zoo(lbp.spread[,-1], lbp.spread[,1])
                
                merge(lbp.zoo, zoo(1, seq(today() - daysback, today(), by = 1)), all = TRUE) %>% 
                    .[,-(ncol(.))] %>%
                    fortify.zoo(.) %>%
                    reshape2::melt("Index") %>%
                    dplyr::rename(date_submitted = Index, original_plan_plus_utility_id = variable, n = value) %>%
                    separate(original_plan_plus_utility_id, into = c("original_plan_id", "utility_id"), sep = "-") -> lbp 
                
                
                
                til.i <- top.impacts.leads[k,]
                ticr.i <- top.impacts.cr[k,]
                
                if(between(til.i$Average_Relative_Effect, ticr.i$Avg_Rel_Eff_Lo_90_Conf_Interval, ticr.i$Avg_Rel_Eff_Up_90_Conf_Interval)){
                    
                    
                    leads.ct[leads.ct$utility_name_id == top.impacts.leads[k, "utility_name_id"],] %>%
                        filter(product == top.impacts.leads[k, "product"]) %>%
                        .[,!names(.) %in% c("utility_name_id", "product")] -> leads.i
                    
                    lbp %>%
                        filter(utility_id == top.impacts.leads[k, "utility_id"]) %>%
                        .[,!names(.) %in% c("utility_id")] %>%
                        spread(key = original_plan_id, value = n, fill = 0) -> lbp.i
                    
                    for.cor.i <- merge(leads.i, lbp.i, by = "date_submitted")
                    
                    arrange(reshape2::melt(cor(for.cor.i$n, for.cor.i[,-1:-2])),desc(value)) %>%
                        .[,!names(.) %in% c("Var1")] %>%
                        dplyr::rename(original_plan_id = Var2, cor = value) -> cor.i
                    
                    lbp.i %>%
                        .[,names(.) %in% c("date_submitted", cor.i$original_plan_id[1:5])] %>%
                        reshape2::melt("date_submitted") %>%
                        dplyr::rename(original_plan_id = variable, n = value) -> top.lbp
                    
                    changes %>%
                        filter(utility_name_id == top.impacts.leads[k, "utility_name_id"]) %>%
                        filter(original_plan_id == as.numeric(cor.i$original_plan_id[1])) -> changes.i
                    
                    changes.i <- arrange(changes.i[!duplicated(changes.i),], plan_audit_id)
                    
                    if(dim(changes.i)[1] == 0){
                        
                        paste0(" ")
                        
                    } else {
                        if(between(changes.i[1,"date"], change.date.i - 3, change.date.i + 3)){
                            
                            as.character(changes.i[1, c("supplier")])
                            
                        } else {
                            paste0(" ")
                        }
                        
                    }
                    
                } else{
                    paste0(" ")
                }
                
            })
            
            
            output$plan_active4.tab <- renderText({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                leads <- get.causal.impact.data()[[7]]
                
                
                k <- 4
                
                change.date.i <- top.impacts.leads[k, "graph.date"]
                
                leads %>%
                    mutate(n = 1) %>%
                    unite(original_plan_plus_utility_id, original_plan_id, utility_id, sep = "-") %>%
                    aggregate(n~original_plan_plus_utility_id + date_submitted, data=., sum) -> leads.by.plan_plus_utility_id     
                
                lbp.spread <- spread(leads.by.plan_plus_utility_id,key = original_plan_plus_utility_id, value = n, fill = 0)
                
                lbp.zoo <- zoo(lbp.spread[,-1], lbp.spread[,1])
                
                merge(lbp.zoo, zoo(1, seq(today() - daysback, today(), by = 1)), all = TRUE) %>% 
                    .[,-(ncol(.))] %>%
                    fortify.zoo(.) %>%
                    reshape2::melt("Index") %>%
                    dplyr::rename(date_submitted = Index, original_plan_plus_utility_id = variable, n = value) %>%
                    separate(original_plan_plus_utility_id, into = c("original_plan_id", "utility_id"), sep = "-") -> lbp 
                
                
                
                til.i <- top.impacts.leads[k,]
                ticr.i <- top.impacts.cr[k,]
                
                if(between(til.i$Average_Relative_Effect, ticr.i$Avg_Rel_Eff_Lo_90_Conf_Interval, ticr.i$Avg_Rel_Eff_Up_90_Conf_Interval)){
                    
                    
                    leads.ct[leads.ct$utility_name_id == top.impacts.leads[k, "utility_name_id"],] %>%
                        filter(product == top.impacts.leads[k, "product"]) %>%
                        .[,!names(.) %in% c("utility_name_id", "product")] -> leads.i
                    
                    lbp %>%
                        filter(utility_id == top.impacts.leads[k, "utility_id"]) %>%
                        .[,!names(.) %in% c("utility_id")] %>%
                        spread(key = original_plan_id, value = n, fill = 0) -> lbp.i
                    
                    for.cor.i <- merge(leads.i, lbp.i, by = "date_submitted")
                    
                    arrange(reshape2::melt(cor(for.cor.i$n, for.cor.i[,-1:-2])),desc(value)) %>%
                        .[,!names(.) %in% c("Var1")] %>%
                        dplyr::rename(original_plan_id = Var2, cor = value) -> cor.i
                    
                    lbp.i %>%
                        .[,names(.) %in% c("date_submitted", cor.i$original_plan_id[1:5])] %>%
                        reshape2::melt("date_submitted") %>%
                        dplyr::rename(original_plan_id = variable, n = value) -> top.lbp
                    
                    changes %>%
                        filter(utility_name_id == top.impacts.leads[k, "utility_name_id"]) %>%
                        filter(original_plan_id == as.numeric(cor.i$original_plan_id[1])) -> changes.i
                    
                    changes.i <- arrange(changes.i[!duplicated(changes.i),], plan_audit_id)
                    
                    if(dim(changes.i)[1] == 0){
                        
                        paste0(" ")
                        
                    } else {
                        if(between(changes.i[1,"date"], change.date.i - 3, change.date.i + 3)){
                            
                            new_plan_status = changes.i[changes.i$plan_audit_id == max(changes.i$plan_audit_id), "plan_status_name"]
                            
                            new_plan_status
                            
                        } else {
                            paste0(" ")
                        }
                        
                    }
                    
                } else{
                    paste0(" ")
                }
                
            })
            
            output$plan_id4.tab <- renderText({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                leads <- get.causal.impact.data()[[7]]
                
                
                k <- 4
                
                change.date.i <- top.impacts.leads[k, "graph.date"]
                
                leads %>%
                    mutate(n = 1) %>%
                    unite(original_plan_plus_utility_id, original_plan_id, utility_id, sep = "-") %>%
                    aggregate(n~original_plan_plus_utility_id + date_submitted, data=., sum) -> leads.by.plan_plus_utility_id     
                
                lbp.spread <- spread(leads.by.plan_plus_utility_id,key = original_plan_plus_utility_id, value = n, fill = 0)
                
                lbp.zoo <- zoo(lbp.spread[,-1], lbp.spread[,1])
                
                merge(lbp.zoo, zoo(1, seq(today() - daysback, today(), by = 1)), all = TRUE) %>% 
                    .[,-(ncol(.))] %>%
                    fortify.zoo(.) %>%
                    reshape2::melt("Index") %>%
                    dplyr::rename(date_submitted = Index, original_plan_plus_utility_id = variable, n = value) %>%
                    separate(original_plan_plus_utility_id, into = c("original_plan_id", "utility_id"), sep = "-") -> lbp 
                
                
                
                til.i <- top.impacts.leads[k,]
                ticr.i <- top.impacts.cr[k,]
                
                if(between(til.i$Average_Relative_Effect, ticr.i$Avg_Rel_Eff_Lo_90_Conf_Interval, ticr.i$Avg_Rel_Eff_Up_90_Conf_Interval)){
                    
                    
                    leads.ct[leads.ct$utility_name_id == top.impacts.leads[k, "utility_name_id"],] %>%
                        filter(product == top.impacts.leads[k, "product"]) %>%
                        .[,!names(.) %in% c("utility_name_id", "product")] -> leads.i
                    
                    lbp %>%
                        filter(utility_id == top.impacts.leads[k, "utility_id"]) %>%
                        .[,!names(.) %in% c("utility_id")] %>%
                        spread(key = original_plan_id, value = n, fill = 0) -> lbp.i
                    
                    for.cor.i <- merge(leads.i, lbp.i, by = "date_submitted")
                    
                    arrange(reshape2::melt(cor(for.cor.i$n, for.cor.i[,-1:-2])),desc(value)) %>%
                        .[,!names(.) %in% c("Var1")] %>%
                        dplyr::rename(original_plan_id = Var2, cor = value) -> cor.i
                    
                    lbp.i %>%
                        .[,names(.) %in% c("date_submitted", cor.i$original_plan_id[1:5])] %>%
                        reshape2::melt("date_submitted") %>%
                        dplyr::rename(original_plan_id = variable, n = value) -> top.lbp
                    
                    changes %>%
                        filter(utility_name_id == top.impacts.leads[k, "utility_name_id"]) %>%
                        filter(original_plan_id == as.numeric(cor.i$original_plan_id[1])) -> changes.i
                    
                    changes.i <- arrange(changes.i[!duplicated(changes.i),], plan_audit_id)
                    
                    
                    if(dim(changes.i)[1] == 0){
                        
                        paste0(" ")
                        
                    } else {
                        if(between(changes.i[1,"date"], change.date.i - 3, change.date.i + 3)){
                            
                            as.character(paste("Plan ID: ", changes.i[1, c("original_plan_id")], sep = ""))
                            
                        } else {
                            paste0(" ")
                        }
                        
                    }
                    
                } else{
                    paste0(" ")
                }
                
                
            })
            
            
            
            output$price_change4 <- renderText({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                leads <- get.causal.impact.data()[[7]]
                
                
                k <- 4
                
                change.date.i <- top.impacts.leads[k, "graph.date"]
                
                leads %>%
                    mutate(n = 1) %>%
                    unite(original_plan_plus_utility_id, original_plan_id, utility_id, sep = "-") %>%
                    aggregate(n~original_plan_plus_utility_id + date_submitted, data=., sum) -> leads.by.plan_plus_utility_id     
                
                lbp.spread <- spread(leads.by.plan_plus_utility_id,key = original_plan_plus_utility_id, value = n, fill = 0)
                
                lbp.zoo <- zoo(lbp.spread[,-1], lbp.spread[,1])
                
                merge(lbp.zoo, zoo(1, seq(today() - daysback, today(), by = 1)), all = TRUE) %>% 
                    .[,-(ncol(.))] %>%
                    fortify.zoo(.) %>%
                    reshape2::melt("Index") %>%
                    dplyr::rename(date_submitted = Index, original_plan_plus_utility_id = variable, n = value) %>%
                    separate(original_plan_plus_utility_id, into = c("original_plan_id", "utility_id"), sep = "-") -> lbp 
                
                
                
                til.i <- top.impacts.leads[k,]
                ticr.i <- top.impacts.cr[k,]
                
                if(between(til.i$Average_Relative_Effect, ticr.i$Avg_Rel_Eff_Lo_90_Conf_Interval, ticr.i$Avg_Rel_Eff_Up_90_Conf_Interval)){
                    
                    
                    leads.ct[leads.ct$utility_name_id == top.impacts.leads[k, "utility_name_id"],] %>%
                        filter(product == top.impacts.leads[k, "product"]) %>%
                        .[,!names(.) %in% c("utility_name_id", "product")] -> leads.i
                    
                    lbp %>%
                        filter(utility_id == top.impacts.leads[k, "utility_id"]) %>%
                        .[,!names(.) %in% c("utility_id")] %>%
                        spread(key = original_plan_id, value = n, fill = 0) -> lbp.i
                    
                    for.cor.i <- merge(leads.i, lbp.i, by = "date_submitted")
                    
                    arrange(reshape2::melt(cor(for.cor.i$n, for.cor.i[,-1:-2])),desc(value)) %>%
                        .[,!names(.) %in% c("Var1")] %>%
                        dplyr::rename(original_plan_id = Var2, cor = value) -> cor.i
                    
                    lbp.i %>%
                        .[,names(.) %in% c("date_submitted", cor.i$original_plan_id[1:5])] %>%
                        reshape2::melt("date_submitted") %>%
                        dplyr::rename(original_plan_id = variable, n = value) -> top.lbp
                    
                    changes %>%
                        filter(utility_name_id == top.impacts.leads[k, "utility_name_id"]) %>%
                        filter(original_plan_id == as.numeric(cor.i$original_plan_id[1])) -> changes.i
                    
                    changes.i <- arrange(changes.i[!duplicated(changes.i),], plan_audit_id)
                    
                    
                    
                    if(dim(changes.i)[1] == 0){
                        
                        paste0(" ")
                        
                    } else {
                        if(between(changes.i[1,"date"], change.date.i - 3, change.date.i + 3)){
                            
                            old_price = changes.i[changes.i$plan_audit_id == min(changes.i$plan_audit_id), "plan_price"]
                            new_price = changes.i[changes.i$plan_audit_id == max(changes.i$plan_audit_id), "plan_price"]
                            
                            paste(old_price, "¢", " -> ", new_price, "¢", sep = "")
                            
                        } else {
                            paste0(" ")
                        }
                        
                    }
                    
                } else{
                    paste0(" ")
                }
                
                
            })
            
            
            
            
            
            
            output$change.4 <- renderInfoBox({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                leads <- get.causal.impact.data()[[7]]
                
                k <- 4
                
                change.date.i <- max(changes[changes$change_index == top.impacts.leads$change_index[k], "date"])
                
                product.i <- unique(changes[changes$change_index == top.impacts.leads$change_index[k], "product"])
                utility.i <- paste(unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility"]), unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility_id"]), sep = " - ")
                
                leads.ct %>%
                    filter(utility_name_id == utility.i) %>%
                    filter(product == product.i) -> leads.i
                
                leads.ts <- zoo(leads.i[,!names(leads.i) %in% c("date_submitted", "utility_name_id", "product")], order.by = leads.i$date_submitted)
                pre.i <- c(min(leads.i$date_submitted), change.date.i)
                post.i <- c(change.date.i + 1, max(leads.i$date_submitted))
                impact.i <- try.Causal.Impact(time_series = leads.ts, pre_date = pre.i, post_date = post.i, conf_int = 0.05)
                
                if(impact.i$summary$AbsEffect[1] < 0){
                    subt <- "Lead Volume Decline"
                    ico <- icon("arrow-down")
                } else {
                    subt <- "Lead Volume Increase"
                    ico <- icon("arrow-up")
                }
                
                infoBox(
                    "What occurred:", subt, icon = ico,
                    color = "purple"
                )
                
            })
            
            output$utility.4 <- renderInfoBox({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                
                k <- 4
                infoBox(
                    "Where:", paste(top.impacts.leads[k, c("state")], top.impacts.leads[k, c("utility")], sep = " - "), icon = icon("plug", "fa-0.5x"),
                    color = "green"
                )        
            })
            
            output$product.4 <- renderInfoBox({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                
                k <- 4
                if(top.impacts.leads[k, c("product")] == "Electricity"){
                    ico <- icon("bolt", "fa-0.5x")
                } else{
                    ico <- icon("fire", "fa-0.5x")
                }
                infoBox(
                    "Which product:", top.impacts.leads[k, c("product")], icon = ico,
                    color = "yellow"
                )        
            })
            
            
            output$causal.impact.cr.plot4 <- renderPlot({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                
                changes.unique <- changes[!duplicated(changes[,c("date", "utility_id")]),]
                unique.index <- unique(changes.unique$change_index)
                
                k <- 4
                
                change.date.i <- max(changes[changes$change_index == top.impacts.leads$change_index[k], "date"])
                
                product.i <- unique(changes[changes$change_index == top.impacts.leads$change_index[k], "product"])
                utility.i <- paste(unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility"]), unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility_id"]), sep = " - ")
                
                cr.ct %>%
                    filter(utility_name_id == utility.i) %>%
                    filter(product == product.i) -> leads.i
                
                leads.ts <- zoo(leads.i[,!names(leads.i) %in% c("date_submitted", "utility_name_id", "product")], order.by = leads.i$date_submitted)
                pre.i <- c(min(leads.i$date_submitted), change.date.i)
                post.i <- c(change.date.i + 1, max(leads.i$date_submitted))
                impact.i <- try.Causal.Impact(time_series = leads.ts, pre_date = pre.i, post_date = post.i, conf_int = 0.05)
                plot(impact.i)
            })
            
            
            output$causal.impact.cr.summary.EXCLUDE <- renderPrint({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                
                changes.unique <- changes[!duplicated(changes[,c("date", "utility_id")]),]
                unique.index <- unique(changes.unique$change_index)
                
                k <- 4
                
                change.date.i <- max(changes[changes$change_index == top.impacts.leads$change_index[k], "date"])
                
                product.i <- unique(changes[changes$change_index == top.impacts.leads$change_index[k], "product"])
                utility.i <- paste(unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility"]), unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility_id"]), sep = " - ")
                
                cr.ct %>%
                    filter(utility_name_id == utility.i) %>%
                    filter(product == product.i) -> leads.i
                
                leads.ts <- zoo(leads.i[,!names(leads.i) %in% c("date_submitted", "utility_name_id", "product")], order.by = leads.i$date_submitted)
                pre.i <- c(min(leads.i$date_submitted), change.date.i)
                post.i <- c(change.date.i + 1, max(leads.i$date_submitted))
                impact.i <- try.Causal.Impact(time_series = leads.ts, pre_date = pre.i, post_date = post.i, conf_int = 0.05)
                summary(impact.i)
            })        
            
            
            output$causal.impact.cr.summary4 <- renderPrint({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                
                changes.unique <- changes[!duplicated(changes[,c("date", "utility_id")]),]
                unique.index <- unique(changes.unique$change_index)
                
                k <- 4
                
                change.date.i <- max(changes[changes$change_index == top.impacts.leads$change_index[k], "date"])
                
                product.i <- unique(changes[changes$change_index == top.impacts.leads$change_index[k], "product"])
                utility.i <- paste(unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility"]), unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility_id"]), sep = " - ")
                cr.ct %>%
                    filter(utility_name_id == utility.i) %>%
                    filter(product == product.i) -> leads.i
                
                leads.ts <- zoo(leads.i[,!names(leads.i) %in% c("date_submitted", "utility_name_id", "product")], order.by = leads.i$date_submitted)
                pre.i <- c(min(leads.i$date_submitted), change.date.i)
                post.i <- c(change.date.i + 1, max(leads.i$date_submitted))
                impact.i <- try.Causal.Impact(time_series = leads.ts, pre_date = pre.i, post_date = post.i, conf_int = 0.05)
                summary(impact.i)
            })
            
            output$causal.impact.leads.summary4 <- renderPrint({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                
                changes.unique <- changes[!duplicated(changes[,c("date", "utility_id")]),]
                unique.index <- unique(changes.unique$change_index)
                
                k <- 4
                
                change.date.i <- max(changes[changes$change_index == top.impacts.leads$change_index[k], "date"])
                
                product.i <- unique(changes[changes$change_index == top.impacts.leads$change_index[k], "product"])
                utility.i <- paste(unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility"]), unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility_id"]), sep = " - ")
                leads.ct %>%
                    filter(utility_name_id == utility.i) %>%
                    filter(product == product.i) -> leads.i
                
                leads.ts <- zoo(leads.i[,!names(leads.i) %in% c("date_submitted", "utility_name_id", "product")], order.by = leads.i$date_submitted)
                pre.i <- c(min(leads.i$date_submitted), change.date.i)
                post.i <- c(change.date.i + 1, max(leads.i$date_submitted))
                impact.i <- try.Causal.Impact(time_series = leads.ts, pre_date = pre.i, post_date = post.i, conf_int = 0.05)
                summary(impact.i)
            })
            
            
            output$causal.impact.leads.plot4 <- renderPlot({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                
                changes.unique <- changes[!duplicated(changes[,c("date", "utility_id")]),]
                unique.index <- unique(changes.unique$change_index)
                
                k <- 4
                
                change.date.i <- max(changes[changes$change_index == top.impacts.leads$change_index[k], "date"])
                
                product.i <- unique(changes[changes$change_index == top.impacts.leads$change_index[k], "product"])
                utility.i <- paste(unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility"]), unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility_id"]), sep = " - ")
                leads.ct %>%
                    filter(utility_name_id == utility.i) %>%
                    filter(product == product.i) -> leads.i
                
                leads.ts <- zoo(leads.i[,!names(leads.i) %in% c("date_submitted", "utility_name_id", "product")], order.by = leads.i$date_submitted)
                pre.i <- c(min(leads.i$date_submitted), change.date.i)
                post.i <- c(change.date.i + 1, max(leads.i$date_submitted))
                impact.i <- try.Causal.Impact(time_series = leads.ts, pre_date = pre.i, post_date = post.i, conf_int = 0.05)
                plot(impact.i)
            })
            
            
            output$complete.plan.changes4 <- renderPlotly({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                
                changes.unique <- changes[!duplicated(changes[,c("date", "utility_id")]),]
                unique.index <- unique(changes.unique$change_index)
                
                k <- 4
                
                change.date.i <- max(changes[changes$change_index == top.impacts.leads$change_index[k], "date"])
                
                product.i <- unique(changes[changes$change_index == top.impacts.leads$change_index[k], "product"])
                utility.i <- paste(unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility"]), unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility_id"]), sep = " - ")
                
                utility.name.i <- unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility_name_id"])
                changes[changes$utility_name_id == utility.name.i, ] %>%
                    filter(graph.date > (today()-(daysback+1))) %>%
                    mutate(Supplier_Plan = paste(change, supplier, plan_name, sep = " - ")) -> changes.full.i
                
                daily.prices %>%
                    filter(utility_name_id == utility.i) %>%
                    filter(product == product.i) -> daily.prices.i
                
                
                
                p <- ggplot() + 
                    geom_line(data = daily.prices.i, aes(date, plan_price.mean)) +
                    geom_line(data = daily.prices.i, aes(date, plan_price.max)) +
                    geom_line(data = daily.prices.i, aes(date, plan_price.min)) +
                    geom_line(data = changes.full.i[changes.full.i$change == "plan_change",], aes(graph.date, plan_price, col = Supplier_Plan)) +
                    geom_point(data = changes.full.i[changes.full.i$change == "plan_end",], aes(graph.date, plan_price, col = Supplier_Plan)) + 
                    geom_point(data = changes.full.i[changes.full.i$change == "new_plan",], aes(graph.date, plan_price, col = Supplier_Plan)) +
                    geom_vline(xintercept = as.numeric(change.date.i), linetype = "longdash") +
                    ggtitle(paste("All Plan Changes Around", change.date.i, sep = " ")) +
                    xlab("Date") +
                    ylab("Plan Price (cents)") +
                    theme(legend.position = "none", panel.background=element_blank(), panel.grid.major = element_line(colour = "grey"))
                
                ggplotly(p)
            })
            
            
            
            
            output$plan_breakdown4 <- renderPlot({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                leads <- get.causal.impact.data()[[7]]
                sessions.ct <- get.causal.impact.data()[[8]]     
                plan.inv <- get.causal.impact.data()[[9]]
                
                
                changes.unique <- changes[!duplicated(changes[,c("date", "utility_id")]),]
                unique.index <- unique(changes.unique$change_index)
                
                k <- 4
                
                change.date.i <- max(changes[changes$change_index == top.impacts.leads$change_index[k], "date"])
                
                product.i <- unique(changes[changes$change_index == top.impacts.leads$change_index[k], "product"])
                utility.i <- paste(unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility"]), unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility_id"]), sep = " - ")
                
                utility.name.i <- unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility_name_id"])
                changes[changes$utility_name_id == utility.name.i, ] %>%
                    filter(graph.date > (today()-(daysback+1))) %>%
                    mutate(suplan = paste(change, supplier, plan_name, sep = " - ")) -> changes.full.i
                
                daily.prices %>%
                    filter(utility_name_id == utility.i) %>%
                    filter(product == product.i) -> daily.prices.i
                
                
                leads %>%
                    mutate(n = 1) %>%
                    unite(original_plan_plus_utility_id, original_plan_id, utility_id, sep = "-") %>%
                    aggregate(n~original_plan_plus_utility_id + date_submitted, data=., sum) -> leads.by.plan_plus_utility_id     
                
                lbp.spread <- spread(leads.by.plan_plus_utility_id,key = original_plan_plus_utility_id, value = n, fill = 0)
                
                lbp.zoo <- zoo(lbp.spread[,-1], lbp.spread[,1])
                
                merge(lbp.zoo, zoo(1, seq(today() - daysback, today(), by = 1)), all = TRUE) %>% 
                    .[,-(ncol(.))] %>%
                    fortify.zoo(.) %>%
                    reshape2::melt("Index") %>%
                    dplyr::rename(date_submitted = Index, original_plan_plus_utility_id = variable, n = value) %>%
                    separate(original_plan_plus_utility_id, into = c("original_plan_id", "utility_id"), sep = "-") -> lbp 
                
                
                
                til.i <- top.impacts.leads[k,]
                ticr.i <- top.impacts.cr[k,]
                
                if(between(til.i$Average_Relative_Effect, ticr.i$Avg_Rel_Eff_Lo_90_Conf_Interval, ticr.i$Avg_Rel_Eff_Up_90_Conf_Interval)){
                    
                    
                    leads.ct[leads.ct$utility_name_id == top.impacts.leads[k, "utility_name_id"],] %>%
                        filter(product == top.impacts.leads[k, "product"]) %>%
                        .[,!names(.) %in% c("utility_name_id", "product")] -> leads.i
                    
                    lbp %>%
                        filter(utility_id == top.impacts.leads[k, "utility_id"]) %>%
                        .[,!names(.) %in% c("utility_id")] %>%
                        spread(key = original_plan_id, value = n, fill = 0) -> lbp.i
                    
                    for.cor.i <- merge(leads.i, lbp.i, by = "date_submitted")
                    
                    arrange(reshape2::melt(cor(for.cor.i$n, for.cor.i[,-1:-2])),desc(value)) %>%
                        .[,!names(.) %in% c("Var1")] %>%
                        dplyr::rename(original_plan_id = Var2, cor = value) -> cor.i
                    
                    lbp.i %>%
                        .[,names(.) %in% c("date_submitted", cor.i$original_plan_id[1:5])] %>%
                        reshape2::melt("date_submitted") %>%
                        dplyr::rename(original_plan_id = variable, n = value) %>%
                        merge(unique(plan.inv[,c("supplier", "plan_name", "original_plan_id")]), by = "original_plan_id", all.x = TRUE) %>%
                        unite(suplan, supplier, plan_name, sep = " - ", remove = TRUE) %>%
                        .[,!names(.) %in% c("original_plan_id")] -> top.lbp
                    
                    changes %>%
                        filter(utility_name_id == top.impacts.leads[k, "utility_name_id"]) %>%
                        filter(original_plan_id == as.numeric(cor.i$original_plan_id[1])) -> changes.i
                    
                    changes.i <- arrange(changes.i[!duplicated(changes.i),], plan_audit_id)
                    
                    leads.i %>%
                        mutate(suplan = "Total Leads") %>%
                        .[,c("date_submitted", "n", "suplan")] %>%
                        rbind(top.lbp) -> leads.for.plot
                    
                    leads.for.plot$suplan <- as.character(leads.for.plot$suplan)
                    leads.for.plot$suplan <- factor(leads.for.plot$suplan, levels = c("Total Leads", unique(top.lbp$suplan)))
                    
                    names(leads.for.plot) <- c("date_submitted", "n", "suplan")
                    
                    ggplot(leads.for.plot, aes(date_submitted, n)) + 
                        geom_line() + 
                        facet_wrap(~suplan, ncol = 1) + 
                        ggtitle("Lead Volume of Top Correlating Plans") +
                        theme(legend.position = "none", panel.background=element_blank(), panel.grid.major = element_line(colour = "grey"), strip.text.y = element_text(angle=0), plot.title = element_text(hjust = 0.5, size = 18))+
                        xlab("Date") +
                        ylab("Lead Volume") +
                        geom_vline(xintercept = as.numeric(change.date.i), linetype = "longdash")
                    
                } else{
                    sessions.ct %>%
                        filter(utility_name_id == top.impacts.leads[k, "utility_name_id"]) %>%
                        aggregate(uniquePageviews ~ date_submitted, data=., sum) -> sessions.for.plot
                    
                    ggplot(sessions.for.plot, aes(date_submitted, uniquePageviews)) + 
                        geom_line() + 
                        ggtitle(paste("Sessions in ", top.impacts.leads[k, "utility"], sep = "")) +
                        theme(legend.position = "none", panel.background=element_blank(), panel.grid.major = element_line(colour = "grey")) +
                        xlab("Date") +
                        ylab("Sessions") +
                        geom_vline(xintercept = as.numeric(change.date.i), linetype = "longdash") +
                        theme(plot.title = element_text(hjust = 0.5, size = 18)) +
                        stat_smooth(method=loess)
                    
                }
                
            })
            
            
            # # # --- # # # --- # # # --- # # # --- # # # ---
            
            #                      TAB 5                    #  
            
            # # # --- # # # --- # # # --- # # # --- # # # ---
            
            
            
            
            
            
            
            output$likelihood.description.5 <- renderText({
                
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                leads <- get.causal.impact.data()[[7]]
                
                
                k <- 5
                
                change.date.i <- top.impacts.leads[k, "graph.date"]
                
                leads %>%
                    mutate(n = 1) %>%
                    unite(original_plan_plus_utility_id, original_plan_id, utility_id, sep = "-") %>%
                    aggregate(n~original_plan_plus_utility_id + date_submitted, data=., sum) -> leads.by.plan_plus_utility_id     
                
                lbp.spread <- spread(leads.by.plan_plus_utility_id,key = original_plan_plus_utility_id, value = n, fill = 0)
                
                lbp.zoo <- zoo(lbp.spread[,-1], lbp.spread[,1])
                
                merge(lbp.zoo, zoo(1, seq(today() - daysback, today(), by = 1)), all = TRUE) %>% 
                    .[,-(ncol(.))] %>%
                    fortify.zoo(.) %>%
                    reshape2::melt("Index") %>%
                    dplyr::rename(date_submitted = Index, original_plan_plus_utility_id = variable, n = value) %>%
                    separate(original_plan_plus_utility_id, into = c("original_plan_id", "utility_id"), sep = "-") -> lbp 
                
                
                til.i <- top.impacts.leads[k,]
                ticr.i <- top.impacts.cr[k,]
                
                if(between(til.i$Average_Relative_Effect, ticr.i$Avg_Rel_Eff_Lo_90_Conf_Interval, ticr.i$Avg_Rel_Eff_Up_90_Conf_Interval)){
                    
                    
                    leads.ct[leads.ct$utility_name_id == top.impacts.leads[k, "utility_name_id"],] %>%
                        filter(product == top.impacts.leads[k, "product"]) %>%
                        .[,!names(.) %in% c("utility_name_id", "product")] -> leads.i
                    
                    lbp %>%
                        filter(utility_id == top.impacts.leads[k, "utility_id"]) %>%
                        .[,!names(.) %in% c("utility_id")] %>%
                        spread(key = original_plan_id, value = n, fill = 0) -> lbp.i
                    
                    for.cor.i <- merge(leads.i, lbp.i, by = "date_submitted")
                    
                    arrange(reshape2::melt(cor(for.cor.i$n, for.cor.i[,-1:-2])),desc(value)) %>%
                        .[,!names(.) %in% c("Var1")] %>%
                        dplyr::rename(original_plan_id = Var2, cor = value) -> cor.i
                    
                    lbp.i %>%
                        .[,names(.) %in% c("date_submitted", cor.i$original_plan_id[1:5])] %>%
                        reshape2::melt("date_submitted") %>%
                        dplyr::rename(original_plan_id = variable, n = value) -> top.lbp
                    
                    changes %>%
                        filter(utility_name_id == top.impacts.leads[k, "utility_name_id"]) %>%
                        filter(original_plan_id == as.numeric(cor.i$original_plan_id[1])) -> changes.i
                    
                    changes.i <- arrange(changes.i[!duplicated(changes.i),], plan_audit_id)
                    
                    if(dim(changes.i)[1] == 0){
                        
                        description <- paste0("Both lead volume and conversion rate were down, although no particular plan changes occurred that could be traced back to. See Lead Volume of Top Correlating Plans below for possibile causes.")
                        
                    } else {
                        if(between(changes.i[1,"date"], change.date.i - 3, change.date.i + 3)){
                            
                            description <- paste0("This plan change is highly likely to have caused the observed change in lead and conversion rate.")
                            
                        } else {
                            description <- paste0("Both lead volume and conversion rate were down, although no particular plan changes occurred that could be traced back to. See Lead Volume of Top Correlating Plans below for possibile causes.")
                        }
                    }
                    
                } else{
                    description <- paste0("No plan changes were found relating to this impact, and conversion rate either stayed relatively the same or went up while leads went down. Therefore the impact can be traced back to a change in traffic in this utility zone.")
                }
                
                description
                
            })
            
            
            
            
            
            output$plan_name5.tab <- renderText({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                leads <- get.causal.impact.data()[[7]]
                
                
                k <- 5
                
                change.date.i <- top.impacts.leads[k, "graph.date"]
                
                leads %>%
                    mutate(n = 1) %>%
                    unite(original_plan_plus_utility_id, original_plan_id, utility_id, sep = "-") %>%
                    aggregate(n~original_plan_plus_utility_id + date_submitted, data=., sum) -> leads.by.plan_plus_utility_id     
                
                lbp.spread <- spread(leads.by.plan_plus_utility_id,key = original_plan_plus_utility_id, value = n, fill = 0)
                
                lbp.zoo <- zoo(lbp.spread[,-1], lbp.spread[,1])
                
                merge(lbp.zoo, zoo(1, seq(today() - daysback, today(), by = 1)), all = TRUE) %>% 
                    .[,-(ncol(.))] %>%
                    fortify.zoo(.) %>%
                    reshape2::melt("Index") %>%
                    dplyr::rename(date_submitted = Index, original_plan_plus_utility_id = variable, n = value) %>%
                    separate(original_plan_plus_utility_id, into = c("original_plan_id", "utility_id"), sep = "-") -> lbp 
                
                
                
                
                
                til.i <- top.impacts.leads[k,]
                ticr.i <- top.impacts.cr[k,]
                
                if(between(til.i$Average_Relative_Effect, ticr.i$Avg_Rel_Eff_Lo_90_Conf_Interval, ticr.i$Avg_Rel_Eff_Up_90_Conf_Interval)){
                    
                    
                    leads.ct[leads.ct$utility_name_id == top.impacts.leads[k, "utility_name_id"],] %>%
                        filter(product == top.impacts.leads[k, "product"]) %>%
                        .[,!names(.) %in% c("utility_name_id", "product")] -> leads.i
                    
                    lbp %>%
                        filter(utility_id == top.impacts.leads[k, "utility_id"]) %>%
                        .[,!names(.) %in% c("utility_id")] %>%
                        spread(key = original_plan_id, value = n, fill = 0) -> lbp.i
                    
                    for.cor.i <- merge(leads.i, lbp.i, by = "date_submitted")
                    
                    arrange(reshape2::melt(cor(for.cor.i$n, for.cor.i[,-1:-2])),desc(value)) %>%
                        .[,!names(.) %in% c("Var1")] %>%
                        dplyr::rename(original_plan_id = Var2, cor = value) -> cor.i
                    
                    lbp.i %>%
                        .[,names(.) %in% c("date_submitted", cor.i$original_plan_id[1:5])] %>%
                        reshape2::melt("date_submitted") %>%
                        dplyr::rename(original_plan_id = variable, n = value) -> top.lbp
                    
                    changes %>%
                        filter(utility_name_id == top.impacts.leads[k, "utility_name_id"]) %>%
                        filter(original_plan_id == as.numeric(cor.i$original_plan_id[1])) -> changes.i
                    
                    changes.i <- arrange(changes.i[!duplicated(changes.i),], plan_audit_id)
                    
                    if(dim(changes.i)[1] == 0){
                        
                        paste0("Multiple Possibilities")
                        
                    } else {
                        if(between(changes.i[1,"date"], change.date.i - 3, change.date.i + 3)){
                            
                            as.character(changes.i[1, c("plan_name")])
                            
                        } else {
                            paste0("Multiple Possibilities")
                        }
                        
                    }
                    
                } else{
                    paste0("Traffic Change")
                }
                
                
                
            })
            
            
            output$plan_term5.tab <- renderText({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                leads <- get.causal.impact.data()[[7]]
                
                
                k <- 5
                
                change.date.i <- top.impacts.leads[k, "graph.date"]
                
                leads %>%
                    mutate(n = 1) %>%
                    unite(original_plan_plus_utility_id, original_plan_id, utility_id, sep = "-") %>%
                    aggregate(n~original_plan_plus_utility_id + date_submitted, data=., sum) -> leads.by.plan_plus_utility_id     
                
                lbp.spread <- spread(leads.by.plan_plus_utility_id,key = original_plan_plus_utility_id, value = n, fill = 0)
                
                lbp.zoo <- zoo(lbp.spread[,-1], lbp.spread[,1])
                
                merge(lbp.zoo, zoo(1, seq(today() - daysback, today(), by = 1)), all = TRUE) %>% 
                    .[,-(ncol(.))] %>%
                    fortify.zoo(.) %>%
                    reshape2::melt("Index") %>%
                    dplyr::rename(date_submitted = Index, original_plan_plus_utility_id = variable, n = value) %>%
                    separate(original_plan_plus_utility_id, into = c("original_plan_id", "utility_id"), sep = "-") -> lbp 
                
                
                
                til.i <- top.impacts.leads[k,]
                ticr.i <- top.impacts.cr[k,]
                
                if(between(til.i$Average_Relative_Effect, ticr.i$Avg_Rel_Eff_Lo_90_Conf_Interval, ticr.i$Avg_Rel_Eff_Up_90_Conf_Interval)){
                    
                    
                    leads.ct[leads.ct$utility_name_id == top.impacts.leads[k, "utility_name_id"],] %>%
                        filter(product == top.impacts.leads[k, "product"]) %>%
                        .[,!names(.) %in% c("utility_name_id", "product")] -> leads.i
                    
                    lbp %>%
                        filter(utility_id == top.impacts.leads[k, "utility_id"]) %>%
                        .[,!names(.) %in% c("utility_id")] %>%
                        spread(key = original_plan_id, value = n, fill = 0) -> lbp.i
                    
                    for.cor.i <- merge(leads.i, lbp.i, by = "date_submitted")
                    
                    arrange(reshape2::melt(cor(for.cor.i$n, for.cor.i[,-1:-2])),desc(value)) %>%
                        .[,!names(.) %in% c("Var1")] %>%
                        dplyr::rename(original_plan_id = Var2, cor = value) -> cor.i
                    
                    lbp.i %>%
                        .[,names(.) %in% c("date_submitted", cor.i$original_plan_id[1:5])] %>%
                        reshape2::melt("date_submitted") %>%
                        dplyr::rename(original_plan_id = variable, n = value) -> top.lbp
                    
                    changes %>%
                        filter(utility_name_id == top.impacts.leads[k, "utility_name_id"]) %>%
                        filter(original_plan_id == as.numeric(cor.i$original_plan_id[1])) -> changes.i
                    
                    changes.i <- arrange(changes.i[!duplicated(changes.i),], plan_audit_id)
                    
                    if(dim(changes.i)[1] == 0){
                        
                        paste0(" ")
                        
                    } else {
                        if(between(changes.i[1,"date"], change.date.i - 3, change.date.i + 3)){
                            
                            as.character(paste(changes.i[1, c("contract_term")], " Months ", changes.i[1, c("contract_type")], sep = ""))
                            
                        } else {
                            paste0(" ")
                        }
                        
                    }
                    
                } else{
                    paste0(" ")
                }
                
            }) 
            
            
            output$plan_supplier5.tab <- renderText({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                leads <- get.causal.impact.data()[[7]]
                
                
                k <- 5
                
                change.date.i <- top.impacts.leads[k, "graph.date"]
                
                leads %>%
                    mutate(n = 1) %>%
                    unite(original_plan_plus_utility_id, original_plan_id, utility_id, sep = "-") %>%
                    aggregate(n~original_plan_plus_utility_id + date_submitted, data=., sum) -> leads.by.plan_plus_utility_id     
                
                lbp.spread <- spread(leads.by.plan_plus_utility_id,key = original_plan_plus_utility_id, value = n, fill = 0)
                
                lbp.zoo <- zoo(lbp.spread[,-1], lbp.spread[,1])
                
                merge(lbp.zoo, zoo(1, seq(today() - daysback, today(), by = 1)), all = TRUE) %>% 
                    .[,-(ncol(.))] %>%
                    fortify.zoo(.) %>%
                    reshape2::melt("Index") %>%
                    dplyr::rename(date_submitted = Index, original_plan_plus_utility_id = variable, n = value) %>%
                    separate(original_plan_plus_utility_id, into = c("original_plan_id", "utility_id"), sep = "-") -> lbp 
                
                
                
                til.i <- top.impacts.leads[k,]
                ticr.i <- top.impacts.cr[k,]
                
                if(between(til.i$Average_Relative_Effect, ticr.i$Avg_Rel_Eff_Lo_90_Conf_Interval, ticr.i$Avg_Rel_Eff_Up_90_Conf_Interval)){
                    
                    
                    leads.ct[leads.ct$utility_name_id == top.impacts.leads[k, "utility_name_id"],] %>%
                        filter(product == top.impacts.leads[k, "product"]) %>%
                        .[,!names(.) %in% c("utility_name_id", "product")] -> leads.i
                    
                    lbp %>%
                        filter(utility_id == top.impacts.leads[k, "utility_id"]) %>%
                        .[,!names(.) %in% c("utility_id")] %>%
                        spread(key = original_plan_id, value = n, fill = 0) -> lbp.i
                    
                    for.cor.i <- merge(leads.i, lbp.i, by = "date_submitted")
                    
                    arrange(reshape2::melt(cor(for.cor.i$n, for.cor.i[,-1:-2])),desc(value)) %>%
                        .[,!names(.) %in% c("Var1")] %>%
                        dplyr::rename(original_plan_id = Var2, cor = value) -> cor.i
                    
                    lbp.i %>%
                        .[,names(.) %in% c("date_submitted", cor.i$original_plan_id[1:5])] %>%
                        reshape2::melt("date_submitted") %>%
                        dplyr::rename(original_plan_id = variable, n = value) -> top.lbp
                    
                    changes %>%
                        filter(utility_name_id == top.impacts.leads[k, "utility_name_id"]) %>%
                        filter(original_plan_id == as.numeric(cor.i$original_plan_id[1])) -> changes.i
                    
                    changes.i <- arrange(changes.i[!duplicated(changes.i),], plan_audit_id)
                    
                    if(dim(changes.i)[1] == 0){
                        
                        paste0(" ")
                        
                    } else {
                        if(between(changes.i[1,"date"], change.date.i - 3, change.date.i + 3)){
                            
                            as.character(changes.i[1, c("supplier")])
                            
                        } else {
                            paste0(" ")
                        }
                        
                    }
                    
                } else{
                    paste0(" ")
                }
                
            })
            
            
            output$plan_active5.tab <- renderText({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                leads <- get.causal.impact.data()[[7]]
                
                
                k <- 5
                
                change.date.i <- top.impacts.leads[k, "graph.date"]
                
                leads %>%
                    mutate(n = 1) %>%
                    unite(original_plan_plus_utility_id, original_plan_id, utility_id, sep = "-") %>%
                    aggregate(n~original_plan_plus_utility_id + date_submitted, data=., sum) -> leads.by.plan_plus_utility_id     
                
                lbp.spread <- spread(leads.by.plan_plus_utility_id,key = original_plan_plus_utility_id, value = n, fill = 0)
                
                lbp.zoo <- zoo(lbp.spread[,-1], lbp.spread[,1])
                
                merge(lbp.zoo, zoo(1, seq(today() - daysback, today(), by = 1)), all = TRUE) %>% 
                    .[,-(ncol(.))] %>%
                    fortify.zoo(.) %>%
                    reshape2::melt("Index") %>%
                    dplyr::rename(date_submitted = Index, original_plan_plus_utility_id = variable, n = value) %>%
                    separate(original_plan_plus_utility_id, into = c("original_plan_id", "utility_id"), sep = "-") -> lbp 
                
                
                
                til.i <- top.impacts.leads[k,]
                ticr.i <- top.impacts.cr[k,]
                
                if(between(til.i$Average_Relative_Effect, ticr.i$Avg_Rel_Eff_Lo_90_Conf_Interval, ticr.i$Avg_Rel_Eff_Up_90_Conf_Interval)){
                    
                    
                    leads.ct[leads.ct$utility_name_id == top.impacts.leads[k, "utility_name_id"],] %>%
                        filter(product == top.impacts.leads[k, "product"]) %>%
                        .[,!names(.) %in% c("utility_name_id", "product")] -> leads.i
                    
                    lbp %>%
                        filter(utility_id == top.impacts.leads[k, "utility_id"]) %>%
                        .[,!names(.) %in% c("utility_id")] %>%
                        spread(key = original_plan_id, value = n, fill = 0) -> lbp.i
                    
                    for.cor.i <- merge(leads.i, lbp.i, by = "date_submitted")
                    
                    arrange(reshape2::melt(cor(for.cor.i$n, for.cor.i[,-1:-2])),desc(value)) %>%
                        .[,!names(.) %in% c("Var1")] %>%
                        dplyr::rename(original_plan_id = Var2, cor = value) -> cor.i
                    
                    lbp.i %>%
                        .[,names(.) %in% c("date_submitted", cor.i$original_plan_id[1:5])] %>%
                        reshape2::melt("date_submitted") %>%
                        dplyr::rename(original_plan_id = variable, n = value) -> top.lbp
                    
                    changes %>%
                        filter(utility_name_id == top.impacts.leads[k, "utility_name_id"]) %>%
                        filter(original_plan_id == as.numeric(cor.i$original_plan_id[1])) -> changes.i
                    
                    changes.i <- arrange(changes.i[!duplicated(changes.i),], plan_audit_id)
                    
                    if(dim(changes.i)[1] == 0){
                        
                        paste0(" ")
                        
                    } else {
                        if(between(changes.i[1,"date"], change.date.i - 3, change.date.i + 3)){
                            
                            new_plan_status = changes.i[changes.i$plan_audit_id == max(changes.i$plan_audit_id), "plan_status_name"]
                            
                            new_plan_status
                            
                        } else {
                            paste0(" ")
                        }
                        
                    }
                    
                } else{
                    paste0(" ")
                }
                
            })
            
            output$plan_id5.tab <- renderText({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                leads <- get.causal.impact.data()[[7]]
                
                
                k <- 5
                
                change.date.i <- top.impacts.leads[k, "graph.date"]
                
                leads %>%
                    mutate(n = 1) %>%
                    unite(original_plan_plus_utility_id, original_plan_id, utility_id, sep = "-") %>%
                    aggregate(n~original_plan_plus_utility_id + date_submitted, data=., sum) -> leads.by.plan_plus_utility_id     
                
                lbp.spread <- spread(leads.by.plan_plus_utility_id,key = original_plan_plus_utility_id, value = n, fill = 0)
                
                lbp.zoo <- zoo(lbp.spread[,-1], lbp.spread[,1])
                
                merge(lbp.zoo, zoo(1, seq(today() - daysback, today(), by = 1)), all = TRUE) %>% 
                    .[,-(ncol(.))] %>%
                    fortify.zoo(.) %>%
                    reshape2::melt("Index") %>%
                    dplyr::rename(date_submitted = Index, original_plan_plus_utility_id = variable, n = value) %>%
                    separate(original_plan_plus_utility_id, into = c("original_plan_id", "utility_id"), sep = "-") -> lbp 
                
                
                
                til.i <- top.impacts.leads[k,]
                ticr.i <- top.impacts.cr[k,]
                
                if(between(til.i$Average_Relative_Effect, ticr.i$Avg_Rel_Eff_Lo_90_Conf_Interval, ticr.i$Avg_Rel_Eff_Up_90_Conf_Interval)){
                    
                    
                    leads.ct[leads.ct$utility_name_id == top.impacts.leads[k, "utility_name_id"],] %>%
                        filter(product == top.impacts.leads[k, "product"]) %>%
                        .[,!names(.) %in% c("utility_name_id", "product")] -> leads.i
                    
                    lbp %>%
                        filter(utility_id == top.impacts.leads[k, "utility_id"]) %>%
                        .[,!names(.) %in% c("utility_id")] %>%
                        spread(key = original_plan_id, value = n, fill = 0) -> lbp.i
                    
                    for.cor.i <- merge(leads.i, lbp.i, by = "date_submitted")
                    
                    arrange(reshape2::melt(cor(for.cor.i$n, for.cor.i[,-1:-2])),desc(value)) %>%
                        .[,!names(.) %in% c("Var1")] %>%
                        dplyr::rename(original_plan_id = Var2, cor = value) -> cor.i
                    
                    lbp.i %>%
                        .[,names(.) %in% c("date_submitted", cor.i$original_plan_id[1:5])] %>%
                        reshape2::melt("date_submitted") %>%
                        dplyr::rename(original_plan_id = variable, n = value) -> top.lbp
                    
                    changes %>%
                        filter(utility_name_id == top.impacts.leads[k, "utility_name_id"]) %>%
                        filter(original_plan_id == as.numeric(cor.i$original_plan_id[1])) -> changes.i
                    
                    changes.i <- arrange(changes.i[!duplicated(changes.i),], plan_audit_id)
                    
                    
                    if(dim(changes.i)[1] == 0){
                        
                        paste0(" ")
                        
                    } else {
                        if(between(changes.i[1,"date"], change.date.i - 3, change.date.i + 3)){
                            
                            as.character(paste("Plan ID: ", changes.i[1, c("original_plan_id")], sep = ""))
                            
                        } else {
                            paste0(" ")
                        }
                        
                    }
                    
                } else{
                    paste0(" ")
                }
                
                
            })
            
            
            
            output$price_change5 <- renderText({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                leads <- get.causal.impact.data()[[7]]
                
                
                k <- 5
                
                change.date.i <- top.impacts.leads[k, "graph.date"]
                
                leads %>%
                    mutate(n = 1) %>%
                    unite(original_plan_plus_utility_id, original_plan_id, utility_id, sep = "-") %>%
                    aggregate(n~original_plan_plus_utility_id + date_submitted, data=., sum) -> leads.by.plan_plus_utility_id     
                
                lbp.spread <- spread(leads.by.plan_plus_utility_id,key = original_plan_plus_utility_id, value = n, fill = 0)
                
                lbp.zoo <- zoo(lbp.spread[,-1], lbp.spread[,1])
                
                merge(lbp.zoo, zoo(1, seq(today() - daysback, today(), by = 1)), all = TRUE) %>% 
                    .[,-(ncol(.))] %>%
                    fortify.zoo(.) %>%
                    reshape2::melt("Index") %>%
                    dplyr::rename(date_submitted = Index, original_plan_plus_utility_id = variable, n = value) %>%
                    separate(original_plan_plus_utility_id, into = c("original_plan_id", "utility_id"), sep = "-") -> lbp 
                
                
                
                til.i <- top.impacts.leads[k,]
                ticr.i <- top.impacts.cr[k,]
                
                if(between(til.i$Average_Relative_Effect, ticr.i$Avg_Rel_Eff_Lo_90_Conf_Interval, ticr.i$Avg_Rel_Eff_Up_90_Conf_Interval)){
                    
                    
                    leads.ct[leads.ct$utility_name_id == top.impacts.leads[k, "utility_name_id"],] %>%
                        filter(product == top.impacts.leads[k, "product"]) %>%
                        .[,!names(.) %in% c("utility_name_id", "product")] -> leads.i
                    
                    lbp %>%
                        filter(utility_id == top.impacts.leads[k, "utility_id"]) %>%
                        .[,!names(.) %in% c("utility_id")] %>%
                        spread(key = original_plan_id, value = n, fill = 0) -> lbp.i
                    
                    for.cor.i <- merge(leads.i, lbp.i, by = "date_submitted")
                    
                    arrange(reshape2::melt(cor(for.cor.i$n, for.cor.i[,-1:-2])),desc(value)) %>%
                        .[,!names(.) %in% c("Var1")] %>%
                        dplyr::rename(original_plan_id = Var2, cor = value) -> cor.i
                    
                    lbp.i %>%
                        .[,names(.) %in% c("date_submitted", cor.i$original_plan_id[1:5])] %>%
                        reshape2::melt("date_submitted") %>%
                        dplyr::rename(original_plan_id = variable, n = value) -> top.lbp
                    
                    changes %>%
                        filter(utility_name_id == top.impacts.leads[k, "utility_name_id"]) %>%
                        filter(original_plan_id == as.numeric(cor.i$original_plan_id[1])) -> changes.i
                    
                    changes.i <- arrange(changes.i[!duplicated(changes.i),], plan_audit_id)
                    
                    
                    
                    if(dim(changes.i)[1] == 0){
                        
                        paste0(" ")
                        
                    } else {
                        if(between(changes.i[1,"date"], change.date.i - 3, change.date.i + 3)){
                            
                            old_price = changes.i[changes.i$plan_audit_id == min(changes.i$plan_audit_id), "plan_price"]
                            new_price = changes.i[changes.i$plan_audit_id == max(changes.i$plan_audit_id), "plan_price"]
                            
                            paste(old_price, "¢", " -> ", new_price, "¢", sep = "")
                            
                        } else {
                            paste0(" ")
                        }
                        
                    }
                    
                } else{
                    paste0(" ")
                }
                
                
            })
            
            
            
            
            
            
            output$change.5 <- renderInfoBox({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                leads <- get.causal.impact.data()[[7]]
                
                k <- 5
                
                change.date.i <- max(changes[changes$change_index == top.impacts.leads$change_index[k], "date"])
                
                product.i <- unique(changes[changes$change_index == top.impacts.leads$change_index[k], "product"])
                utility.i <- paste(unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility"]), unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility_id"]), sep = " - ")
                
                leads.ct %>%
                    filter(utility_name_id == utility.i) %>%
                    filter(product == product.i) -> leads.i
                
                leads.ts <- zoo(leads.i[,!names(leads.i) %in% c("date_submitted", "utility_name_id", "product")], order.by = leads.i$date_submitted)
                pre.i <- c(min(leads.i$date_submitted), change.date.i)
                post.i <- c(change.date.i + 1, max(leads.i$date_submitted))
                impact.i <- try.Causal.Impact(time_series = leads.ts, pre_date = pre.i, post_date = post.i, conf_int = 0.05)
                
                if(impact.i$summary$AbsEffect[1] < 0){
                    subt <- "Lead Volume Decline"
                    ico <- icon("arrow-down")
                } else {
                    subt <- "Lead Volume Increase"
                    ico <- icon("arrow-up")
                }
                
                infoBox(
                    "What occurred:", subt, icon = ico,
                    color = "purple"
                )
                
            })
            
            output$utility.5 <- renderInfoBox({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                
                k <- 5
                infoBox(
                    "Where:", paste(top.impacts.leads[k, c("state")], top.impacts.leads[k, c("utility")], sep = " - "), icon = icon("plug", "fa-0.5x"),
                    color = "green"
                )        
            })
            
            output$product.5 <- renderInfoBox({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                
                k <- 5
                if(top.impacts.leads[k, c("product")] == "Electricity"){
                    ico <- icon("bolt", "fa-0.5x")
                } else{
                    ico <- icon("fire", "fa-0.5x")
                }
                infoBox(
                    "Which product:", top.impacts.leads[k, c("product")], icon = ico,
                    color = "yellow"
                )        
            })
            
            
            output$causal.impact.cr.plot5 <- renderPlot({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                
                changes.unique <- changes[!duplicated(changes[,c("date", "utility_id")]),]
                unique.index <- unique(changes.unique$change_index)
                
                k <- 5
                
                change.date.i <- max(changes[changes$change_index == top.impacts.leads$change_index[k], "date"])
                
                product.i <- unique(changes[changes$change_index == top.impacts.leads$change_index[k], "product"])
                utility.i <- paste(unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility"]), unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility_id"]), sep = " - ")
                
                cr.ct %>%
                    filter(utility_name_id == utility.i) %>%
                    filter(product == product.i) -> leads.i
                
                leads.ts <- zoo(leads.i[,!names(leads.i) %in% c("date_submitted", "utility_name_id", "product")], order.by = leads.i$date_submitted)
                pre.i <- c(min(leads.i$date_submitted), change.date.i)
                post.i <- c(change.date.i + 1, max(leads.i$date_submitted))
                impact.i <- try.Causal.Impact(time_series = leads.ts, pre_date = pre.i, post_date = post.i, conf_int = 0.05)
                plot(impact.i)
            })
            
            
            output$causal.impact.cr.summary.EXCLUDE <- renderPrint({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                
                changes.unique <- changes[!duplicated(changes[,c("date", "utility_id")]),]
                unique.index <- unique(changes.unique$change_index)
                
                k <- 5
                
                change.date.i <- max(changes[changes$change_index == top.impacts.leads$change_index[k], "date"])
                
                product.i <- unique(changes[changes$change_index == top.impacts.leads$change_index[k], "product"])
                utility.i <- paste(unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility"]), unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility_id"]), sep = " - ")
                
                cr.ct %>%
                    filter(utility_name_id == utility.i) %>%
                    filter(product == product.i) -> leads.i
                
                leads.ts <- zoo(leads.i[,!names(leads.i) %in% c("date_submitted", "utility_name_id", "product")], order.by = leads.i$date_submitted)
                pre.i <- c(min(leads.i$date_submitted), change.date.i)
                post.i <- c(change.date.i + 1, max(leads.i$date_submitted))
                impact.i <- try.Causal.Impact(time_series = leads.ts, pre_date = pre.i, post_date = post.i, conf_int = 0.05)
                summary(impact.i)
            })        
            
            
            output$causal.impact.cr.summary5 <- renderPrint({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                
                changes.unique <- changes[!duplicated(changes[,c("date", "utility_id")]),]
                unique.index <- unique(changes.unique$change_index)
                
                k <- 5
                
                change.date.i <- max(changes[changes$change_index == top.impacts.leads$change_index[k], "date"])
                
                product.i <- unique(changes[changes$change_index == top.impacts.leads$change_index[k], "product"])
                utility.i <- paste(unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility"]), unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility_id"]), sep = " - ")
                cr.ct %>%
                    filter(utility_name_id == utility.i) %>%
                    filter(product == product.i) -> leads.i
                
                leads.ts <- zoo(leads.i[,!names(leads.i) %in% c("date_submitted", "utility_name_id", "product")], order.by = leads.i$date_submitted)
                pre.i <- c(min(leads.i$date_submitted), change.date.i)
                post.i <- c(change.date.i + 1, max(leads.i$date_submitted))
                impact.i <- try.Causal.Impact(time_series = leads.ts, pre_date = pre.i, post_date = post.i, conf_int = 0.05)
                summary(impact.i)
            })
            
            output$causal.impact.leads.summary5 <- renderPrint({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                
                changes.unique <- changes[!duplicated(changes[,c("date", "utility_id")]),]
                unique.index <- unique(changes.unique$change_index)
                
                k <- 5
                
                change.date.i <- max(changes[changes$change_index == top.impacts.leads$change_index[k], "date"])
                
                product.i <- unique(changes[changes$change_index == top.impacts.leads$change_index[k], "product"])
                utility.i <- paste(unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility"]), unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility_id"]), sep = " - ")
                leads.ct %>%
                    filter(utility_name_id == utility.i) %>%
                    filter(product == product.i) -> leads.i
                
                leads.ts <- zoo(leads.i[,!names(leads.i) %in% c("date_submitted", "utility_name_id", "product")], order.by = leads.i$date_submitted)
                pre.i <- c(min(leads.i$date_submitted), change.date.i)
                post.i <- c(change.date.i + 1, max(leads.i$date_submitted))
                impact.i <- try.Causal.Impact(time_series = leads.ts, pre_date = pre.i, post_date = post.i, conf_int = 0.05)
                summary(impact.i)
            })
            
            
            output$causal.impact.leads.plot5 <- renderPlot({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                
                changes.unique <- changes[!duplicated(changes[,c("date", "utility_id")]),]
                unique.index <- unique(changes.unique$change_index)
                
                k <- 5
                
                change.date.i <- max(changes[changes$change_index == top.impacts.leads$change_index[k], "date"])
                
                product.i <- unique(changes[changes$change_index == top.impacts.leads$change_index[k], "product"])
                utility.i <- paste(unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility"]), unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility_id"]), sep = " - ")
                leads.ct %>%
                    filter(utility_name_id == utility.i) %>%
                    filter(product == product.i) -> leads.i
                
                leads.ts <- zoo(leads.i[,!names(leads.i) %in% c("date_submitted", "utility_name_id", "product")], order.by = leads.i$date_submitted)
                pre.i <- c(min(leads.i$date_submitted), change.date.i)
                post.i <- c(change.date.i + 1, max(leads.i$date_submitted))
                impact.i <- try.Causal.Impact(time_series = leads.ts, pre_date = pre.i, post_date = post.i, conf_int = 0.05)
                plot(impact.i)
            })
            
            
            output$complete.plan.changes5 <- renderPlotly({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                
                changes.unique <- changes[!duplicated(changes[,c("date", "utility_id")]),]
                unique.index <- unique(changes.unique$change_index)
                
                k <- 5
                
                change.date.i <- max(changes[changes$change_index == top.impacts.leads$change_index[k], "date"])
                
                product.i <- unique(changes[changes$change_index == top.impacts.leads$change_index[k], "product"])
                utility.i <- paste(unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility"]), unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility_id"]), sep = " - ")
                
                utility.name.i <- unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility_name_id"])
                changes[changes$utility_name_id == utility.name.i, ] %>%
                    filter(graph.date > (today()-(daysback+1))) %>%
                    mutate(Supplier_Plan = paste(change, supplier, plan_name, sep = " - ")) -> changes.full.i
                
                daily.prices %>%
                    filter(utility_name_id == utility.i) %>%
                    filter(product == product.i) -> daily.prices.i
                
                
                
                p <- ggplot() + 
                    geom_line(data = daily.prices.i, aes(date, plan_price.mean)) +
                    geom_line(data = daily.prices.i, aes(date, plan_price.max)) +
                    geom_line(data = daily.prices.i, aes(date, plan_price.min)) +
                    geom_line(data = changes.full.i[changes.full.i$change == "plan_change",], aes(graph.date, plan_price, col = Supplier_Plan)) +
                    geom_point(data = changes.full.i[changes.full.i$change == "plan_end",], aes(graph.date, plan_price, col = Supplier_Plan)) + 
                    geom_point(data = changes.full.i[changes.full.i$change == "new_plan",], aes(graph.date, plan_price, col = Supplier_Plan)) +
                    geom_vline(xintercept = as.numeric(change.date.i), linetype = "longdash") +
                    ggtitle(paste("All Plan Changes Around", change.date.i, sep = " ")) +
                    xlab("Date") +
                    ylab("Plan Price (cents)") +
                    theme(legend.position = "none", panel.background=element_blank(), panel.grid.major = element_line(colour = "grey"))
                
                ggplotly(p)
            })
            
            
            
            
            
            output$plan_breakdown5 <- renderPlot({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                leads <- get.causal.impact.data()[[7]]
                sessions.ct <- get.causal.impact.data()[[8]]     
                plan.inv <- get.causal.impact.data()[[9]]
                
                
                changes.unique <- changes[!duplicated(changes[,c("date", "utility_id")]),]
                unique.index <- unique(changes.unique$change_index)
                
                k <- 5
                
                change.date.i <- max(changes[changes$change_index == top.impacts.leads$change_index[k], "date"])
                
                product.i <- unique(changes[changes$change_index == top.impacts.leads$change_index[k], "product"])
                utility.i <- paste(unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility"]), unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility_id"]), sep = " - ")
                
                utility.name.i <- unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility_name_id"])
                changes[changes$utility_name_id == utility.name.i, ] %>%
                    filter(graph.date > (today()-(daysback+1))) %>%
                    mutate(suplan = paste(change, supplier, plan_name, sep = " - ")) -> changes.full.i
                
                daily.prices %>%
                    filter(utility_name_id == utility.i) %>%
                    filter(product == product.i) -> daily.prices.i
                
                
                leads %>%
                    mutate(n = 1) %>%
                    unite(original_plan_plus_utility_id, original_plan_id, utility_id, sep = "-") %>%
                    aggregate(n~original_plan_plus_utility_id + date_submitted, data=., sum) -> leads.by.plan_plus_utility_id     
                
                lbp.spread <- spread(leads.by.plan_plus_utility_id,key = original_plan_plus_utility_id, value = n, fill = 0)
                
                lbp.zoo <- zoo(lbp.spread[,-1], lbp.spread[,1])
                
                merge(lbp.zoo, zoo(1, seq(today() - daysback, today(), by = 1)), all = TRUE) %>% 
                    .[,-(ncol(.))] %>%
                    fortify.zoo(.) %>%
                    reshape2::melt("Index") %>%
                    dplyr::rename(date_submitted = Index, original_plan_plus_utility_id = variable, n = value) %>%
                    separate(original_plan_plus_utility_id, into = c("original_plan_id", "utility_id"), sep = "-") -> lbp 
                
                
                
                til.i <- top.impacts.leads[k,]
                ticr.i <- top.impacts.cr[k,]
                
                if(between(til.i$Average_Relative_Effect, ticr.i$Avg_Rel_Eff_Lo_90_Conf_Interval, ticr.i$Avg_Rel_Eff_Up_90_Conf_Interval)){
                    
                    
                    leads.ct[leads.ct$utility_name_id == top.impacts.leads[k, "utility_name_id"],] %>%
                        filter(product == top.impacts.leads[k, "product"]) %>%
                        .[,!names(.) %in% c("utility_name_id", "product")] -> leads.i
                    
                    lbp %>%
                        filter(utility_id == top.impacts.leads[k, "utility_id"]) %>%
                        .[,!names(.) %in% c("utility_id")] %>%
                        spread(key = original_plan_id, value = n, fill = 0) -> lbp.i
                    
                    for.cor.i <- merge(leads.i, lbp.i, by = "date_submitted")
                    
                    arrange(reshape2::melt(cor(for.cor.i$n, for.cor.i[,-1:-2])),desc(value)) %>%
                        .[,!names(.) %in% c("Var1")] %>%
                        dplyr::rename(original_plan_id = Var2, cor = value) -> cor.i
                    
                    lbp.i %>%
                        .[,names(.) %in% c("date_submitted", cor.i$original_plan_id[1:5])] %>%
                        reshape2::melt("date_submitted") %>%
                        dplyr::rename(original_plan_id = variable, n = value) %>%
                        merge(unique(plan.inv[,c("supplier", "plan_name", "original_plan_id")]), by = "original_plan_id", all.x = TRUE) %>%
                        unite(suplan, supplier, plan_name, sep = " - ", remove = TRUE) %>%
                        .[,!names(.) %in% c("original_plan_id")] -> top.lbp
                    
                    changes %>%
                        filter(utility_name_id == top.impacts.leads[k, "utility_name_id"]) %>%
                        filter(original_plan_id == as.numeric(cor.i$original_plan_id[1])) -> changes.i
                    
                    changes.i <- arrange(changes.i[!duplicated(changes.i),], plan_audit_id)
                    
                    leads.i %>%
                        mutate(suplan = "Total Leads") %>%
                        .[,c("date_submitted", "n", "suplan")] %>%
                        rbind(top.lbp) -> leads.for.plot
                    
                    leads.for.plot$suplan <- as.character(leads.for.plot$suplan)
                    leads.for.plot$suplan <- factor(leads.for.plot$suplan, levels = c("Total Leads", unique(top.lbp$suplan)))
                    
                    names(leads.for.plot) <- c("date_submitted", "n", "suplan")
                    
                    ggplot(leads.for.plot, aes(date_submitted, n)) + 
                        geom_line() + 
                        facet_wrap(~suplan, ncol = 1) + 
                        ggtitle("Lead Volume of Top Correlating Plans") +
                        theme(legend.position = "none", panel.background=element_blank(), panel.grid.major = element_line(colour = "grey"), strip.text.y = element_text(angle=0), plot.title = element_text(hjust = 0.5, size = 18))+
                        xlab("Date") +
                        ylab("Lead Volume") +
                        geom_vline(xintercept = as.numeric(change.date.i), linetype = "longdash")
                    
                } else{
                    sessions.ct %>%
                        filter(utility_name_id == top.impacts.leads[k, "utility_name_id"]) %>%
                        aggregate(uniquePageviews ~ date_submitted, data=., sum) -> sessions.for.plot
                    
                    ggplot(sessions.for.plot, aes(date_submitted, uniquePageviews)) + 
                        geom_line() + 
                        ggtitle(paste("Sessions in ", top.impacts.leads[k, "utility"], sep = "")) +
                        theme(legend.position = "none", panel.background=element_blank(), panel.grid.major = element_line(colour = "grey")) +
                        xlab("Date") +
                        ylab("Sessions") +
                        geom_vline(xintercept = as.numeric(change.date.i), linetype = "longdash") +
                        theme(plot.title = element_text(hjust = 0.5, size = 18)) +
                        stat_smooth(method=loess)
                    
                }
                
                
            })
            
            
            # # # --- # # # --- # # # --- # # # --- # # # ---
            
            #                      TAB 6                    #  
            
            # # # --- # # # --- # # # --- # # # --- # # # ---
            
            
            
            
            
            
            
            output$likelihood.description.6 <- renderText({
                
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                leads <- get.causal.impact.data()[[7]]
                
                
                k <- 6
                
                change.date.i <- top.impacts.leads[k, "graph.date"]
                
                leads %>%
                    mutate(n = 1) %>%
                    unite(original_plan_plus_utility_id, original_plan_id, utility_id, sep = "-") %>%
                    aggregate(n~original_plan_plus_utility_id + date_submitted, data=., sum) -> leads.by.plan_plus_utility_id     
                
                lbp.spread <- spread(leads.by.plan_plus_utility_id,key = original_plan_plus_utility_id, value = n, fill = 0)
                
                lbp.zoo <- zoo(lbp.spread[,-1], lbp.spread[,1])
                
                merge(lbp.zoo, zoo(1, seq(today() - daysback, today(), by = 1)), all = TRUE) %>% 
                    .[,-(ncol(.))] %>%
                    fortify.zoo(.) %>%
                    reshape2::melt("Index") %>%
                    dplyr::rename(date_submitted = Index, original_plan_plus_utility_id = variable, n = value) %>%
                    separate(original_plan_plus_utility_id, into = c("original_plan_id", "utility_id"), sep = "-") -> lbp 
                
                
                til.i <- top.impacts.leads[k,]
                ticr.i <- top.impacts.cr[k,]
                
                if(between(til.i$Average_Relative_Effect, ticr.i$Avg_Rel_Eff_Lo_90_Conf_Interval, ticr.i$Avg_Rel_Eff_Up_90_Conf_Interval)){
                    
                    
                    leads.ct[leads.ct$utility_name_id == top.impacts.leads[k, "utility_name_id"],] %>%
                        filter(product == top.impacts.leads[k, "product"]) %>%
                        .[,!names(.) %in% c("utility_name_id", "product")] -> leads.i
                    
                    lbp %>%
                        filter(utility_id == top.impacts.leads[k, "utility_id"]) %>%
                        .[,!names(.) %in% c("utility_id")] %>%
                        spread(key = original_plan_id, value = n, fill = 0) -> lbp.i
                    
                    for.cor.i <- merge(leads.i, lbp.i, by = "date_submitted")
                    
                    arrange(reshape2::melt(cor(for.cor.i$n, for.cor.i[,-1:-2])),desc(value)) %>%
                        .[,!names(.) %in% c("Var1")] %>%
                        dplyr::rename(original_plan_id = Var2, cor = value) -> cor.i
                    
                    lbp.i %>%
                        .[,names(.) %in% c("date_submitted", cor.i$original_plan_id[1:5])] %>%
                        reshape2::melt("date_submitted") %>%
                        dplyr::rename(original_plan_id = variable, n = value) -> top.lbp
                    
                    changes %>%
                        filter(utility_name_id == top.impacts.leads[k, "utility_name_id"]) %>%
                        filter(original_plan_id == as.numeric(cor.i$original_plan_id[1])) -> changes.i
                    
                    changes.i <- arrange(changes.i[!duplicated(changes.i),], plan_audit_id)
                    
                    if(dim(changes.i)[1] == 0){
                        
                        description <- paste0("Both lead volume and conversion rate were down, although no particular plan changes occurred that could be traced back to. See Lead Volume of Top Correlating Plans below for possibile causes.")
                        
                    } else {
                        if(between(changes.i[1,"date"], change.date.i - 3, change.date.i + 3)){
                            
                            description <- paste0("This plan change is highly likely to have caused the observed change in lead and conversion rate.")
                            
                        } else {
                            description <- paste0("Both lead volume and conversion rate were down, although no particular plan changes occurred that could be traced back to. See Lead Volume of Top Correlating Plans below for possibile causes.")
                        }
                    }
                    
                } else{
                    description <- paste0("No plan changes were found relating to this impact, and conversion rate either stayed relatively the same or went up while leads went down. Therefore the impact can be traced back to a change in traffic in this utility zone.")
                }
                
                description
                
            })
            
            
            
            
            
            output$plan_name6.tab <- renderText({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                leads <- get.causal.impact.data()[[7]]
                
                
                k <- 6
                
                change.date.i <- top.impacts.leads[k, "graph.date"]
                
                leads %>%
                    mutate(n = 1) %>%
                    unite(original_plan_plus_utility_id, original_plan_id, utility_id, sep = "-") %>%
                    aggregate(n~original_plan_plus_utility_id + date_submitted, data=., sum) -> leads.by.plan_plus_utility_id     
                
                lbp.spread <- spread(leads.by.plan_plus_utility_id,key = original_plan_plus_utility_id, value = n, fill = 0)
                
                lbp.zoo <- zoo(lbp.spread[,-1], lbp.spread[,1])
                
                merge(lbp.zoo, zoo(1, seq(today() - daysback, today(), by = 1)), all = TRUE) %>% 
                    .[,-(ncol(.))] %>%
                    fortify.zoo(.) %>%
                    reshape2::melt("Index") %>%
                    dplyr::rename(date_submitted = Index, original_plan_plus_utility_id = variable, n = value) %>%
                    separate(original_plan_plus_utility_id, into = c("original_plan_id", "utility_id"), sep = "-") -> lbp 

                til.i <- top.impacts.leads[k,]
                ticr.i <- top.impacts.cr[k,]
                
                if(between(til.i$Average_Relative_Effect, ticr.i$Avg_Rel_Eff_Lo_90_Conf_Interval, ticr.i$Avg_Rel_Eff_Up_90_Conf_Interval)){
                    
                    
                    leads.ct[leads.ct$utility_name_id == top.impacts.leads[k, "utility_name_id"],] %>%
                        filter(product == top.impacts.leads[k, "product"]) %>%
                        .[,!names(.) %in% c("utility_name_id", "product")] -> leads.i
                    
                    lbp %>%
                        filter(utility_id == top.impacts.leads[k, "utility_id"]) %>%
                        .[,!names(.) %in% c("utility_id")] %>%
                        spread(key = original_plan_id, value = n, fill = 0) -> lbp.i
                    
                    for.cor.i <- merge(leads.i, lbp.i, by = "date_submitted")
                    
                    arrange(reshape2::melt(cor(for.cor.i$n, for.cor.i[,-1:-2])),desc(value)) %>%
                        .[,!names(.) %in% c("Var1")] %>%
                        dplyr::rename(original_plan_id = Var2, cor = value) -> cor.i
                    
                    lbp.i %>%
                        .[,names(.) %in% c("date_submitted", cor.i$original_plan_id[1:5])] %>%
                        reshape2::melt("date_submitted") %>%
                        dplyr::rename(original_plan_id = variable, n = value) -> top.lbp
                    
                    changes %>%
                        filter(utility_name_id == top.impacts.leads[k, "utility_name_id"]) %>%
                        filter(original_plan_id == as.numeric(cor.i$original_plan_id[1])) -> changes.i
                    
                    changes.i <- arrange(changes.i[!duplicated(changes.i),], plan_audit_id)
                
                    
                    if(dim(changes.i)[1] == 0){
                        
                        paste0("Multiple Possibilities")
                        
                    } else {
                        if(between(changes.i[1,"date"], change.date.i - 3, change.date.i + 3)){
                            
                            as.character(changes.i[1, c("plan_name")])
                            
                        } else {
                            paste0("Multiple Possibilities")
                        }
                        
                    }
                    
                } else{
                    paste0("Traffic Change")
                }
                
                
                
            })
            
            
            output$plan_term6.tab <- renderText({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                leads <- get.causal.impact.data()[[7]]
                
                
                k <- 6
                
                change.date.i <- top.impacts.leads[k, "graph.date"]
                
                leads %>%
                    mutate(n = 1) %>%
                    unite(original_plan_plus_utility_id, original_plan_id, utility_id, sep = "-") %>%
                    aggregate(n~original_plan_plus_utility_id + date_submitted, data=., sum) -> leads.by.plan_plus_utility_id     
                
                lbp.spread <- spread(leads.by.plan_plus_utility_id,key = original_plan_plus_utility_id, value = n, fill = 0)
                
                lbp.zoo <- zoo(lbp.spread[,-1], lbp.spread[,1])
                
                merge(lbp.zoo, zoo(1, seq(today() - daysback, today(), by = 1)), all = TRUE) %>% 
                    .[,-(ncol(.))] %>%
                    fortify.zoo(.) %>%
                    reshape2::melt("Index") %>%
                    dplyr::rename(date_submitted = Index, original_plan_plus_utility_id = variable, n = value) %>%
                    separate(original_plan_plus_utility_id, into = c("original_plan_id", "utility_id"), sep = "-") -> lbp 
                
                
                
                til.i <- top.impacts.leads[k,]
                ticr.i <- top.impacts.cr[k,]
                
                if(between(til.i$Average_Relative_Effect, ticr.i$Avg_Rel_Eff_Lo_90_Conf_Interval, ticr.i$Avg_Rel_Eff_Up_90_Conf_Interval)){
                    
                    
                    leads.ct[leads.ct$utility_name_id == top.impacts.leads[k, "utility_name_id"],] %>%
                        filter(product == top.impacts.leads[k, "product"]) %>%
                        .[,!names(.) %in% c("utility_name_id", "product")] -> leads.i
                    
                    lbp %>%
                        filter(utility_id == top.impacts.leads[k, "utility_id"]) %>%
                        .[,!names(.) %in% c("utility_id")] %>%
                        spread(key = original_plan_id, value = n, fill = 0) -> lbp.i
                    
                    for.cor.i <- merge(leads.i, lbp.i, by = "date_submitted")
                    
                    arrange(reshape2::melt(cor(for.cor.i$n, for.cor.i[,-1:-2])),desc(value)) %>%
                        .[,!names(.) %in% c("Var1")] %>%
                        dplyr::rename(original_plan_id = Var2, cor = value) -> cor.i
                    
                    lbp.i %>%
                        .[,names(.) %in% c("date_submitted", cor.i$original_plan_id[1:5])] %>%
                        reshape2::melt("date_submitted") %>%
                        dplyr::rename(original_plan_id = variable, n = value) -> top.lbp
                    
                    changes %>%
                        filter(utility_name_id == top.impacts.leads[k, "utility_name_id"]) %>%
                        filter(original_plan_id == as.numeric(cor.i$original_plan_id[1])) -> changes.i
                    
                    changes.i <- arrange(changes.i[!duplicated(changes.i),], plan_audit_id)
                    
                    if(dim(changes.i)[1] == 0){
                        
                        paste0(" ")
                        
                    } else {
                        if(between(changes.i[1,"date"], change.date.i - 3, change.date.i + 3)){
                            
                            as.character(paste(changes.i[1, c("contract_term")], " Months ", changes.i[1, c("contract_type")], sep = ""))
                            
                        } else {
                            paste0(" ")
                        }
                        
                    }
                    
                } else{
                    paste0(" ")
                }
                
            }) 
            
            
            output$plan_supplier6.tab <- renderText({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                leads <- get.causal.impact.data()[[7]]
                
                
                k <- 6
                
                change.date.i <- top.impacts.leads[k, "graph.date"]
                
                leads %>%
                    mutate(n = 1) %>%
                    unite(original_plan_plus_utility_id, original_plan_id, utility_id, sep = "-") %>%
                    aggregate(n~original_plan_plus_utility_id + date_submitted, data=., sum) -> leads.by.plan_plus_utility_id     
                
                lbp.spread <- spread(leads.by.plan_plus_utility_id,key = original_plan_plus_utility_id, value = n, fill = 0)
                
                lbp.zoo <- zoo(lbp.spread[,-1], lbp.spread[,1])
                
                merge(lbp.zoo, zoo(1, seq(today() - daysback, today(), by = 1)), all = TRUE) %>% 
                    .[,-(ncol(.))] %>%
                    fortify.zoo(.) %>%
                    reshape2::melt("Index") %>%
                    dplyr::rename(date_submitted = Index, original_plan_plus_utility_id = variable, n = value) %>%
                    separate(original_plan_plus_utility_id, into = c("original_plan_id", "utility_id"), sep = "-") -> lbp 
                
                
                
                til.i <- top.impacts.leads[k,]
                ticr.i <- top.impacts.cr[k,]
                
                if(between(til.i$Average_Relative_Effect, ticr.i$Avg_Rel_Eff_Lo_90_Conf_Interval, ticr.i$Avg_Rel_Eff_Up_90_Conf_Interval)){
                    
                    
                    leads.ct[leads.ct$utility_name_id == top.impacts.leads[k, "utility_name_id"],] %>%
                        filter(product == top.impacts.leads[k, "product"]) %>%
                        .[,!names(.) %in% c("utility_name_id", "product")] -> leads.i
                    
                    lbp %>%
                        filter(utility_id == top.impacts.leads[k, "utility_id"]) %>%
                        .[,!names(.) %in% c("utility_id")] %>%
                        spread(key = original_plan_id, value = n, fill = 0) -> lbp.i
                    
                    for.cor.i <- merge(leads.i, lbp.i, by = "date_submitted")
                    
                    arrange(reshape2::melt(cor(for.cor.i$n, for.cor.i[,-1:-2])),desc(value)) %>%
                        .[,!names(.) %in% c("Var1")] %>%
                        dplyr::rename(original_plan_id = Var2, cor = value) -> cor.i
                    
                    lbp.i %>%
                        .[,names(.) %in% c("date_submitted", cor.i$original_plan_id[1:5])] %>%
                        reshape2::melt("date_submitted") %>%
                        dplyr::rename(original_plan_id = variable, n = value) -> top.lbp
                    
                    changes %>%
                        filter(utility_name_id == top.impacts.leads[k, "utility_name_id"]) %>%
                        filter(original_plan_id == as.numeric(cor.i$original_plan_id[1])) -> changes.i
                    
                    changes.i <- arrange(changes.i[!duplicated(changes.i),], plan_audit_id)
                    
                    if(dim(changes.i)[1] == 0){
                        
                        paste0(" ")
                        
                    } else {
                        if(between(changes.i[1,"date"], change.date.i - 3, change.date.i + 3)){
                            
                            as.character(changes.i[1, c("supplier")])
                            
                        } else {
                            paste0(" ")
                        }
                        
                    }
                    
                } else{
                    paste0(" ")
                }
                
            })
            
            
            output$plan_active6.tab <- renderText({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                leads <- get.causal.impact.data()[[7]]
                
                
                k <- 6
                
                change.date.i <- top.impacts.leads[k, "graph.date"]
                
                leads %>%
                    mutate(n = 1) %>%
                    unite(original_plan_plus_utility_id, original_plan_id, utility_id, sep = "-") %>%
                    aggregate(n~original_plan_plus_utility_id + date_submitted, data=., sum) -> leads.by.plan_plus_utility_id     
                
                lbp.spread <- spread(leads.by.plan_plus_utility_id,key = original_plan_plus_utility_id, value = n, fill = 0)
                
                lbp.zoo <- zoo(lbp.spread[,-1], lbp.spread[,1])
                
                merge(lbp.zoo, zoo(1, seq(today() - daysback, today(), by = 1)), all = TRUE) %>% 
                    .[,-(ncol(.))] %>%
                    fortify.zoo(.) %>%
                    reshape2::melt("Index") %>%
                    dplyr::rename(date_submitted = Index, original_plan_plus_utility_id = variable, n = value) %>%
                    separate(original_plan_plus_utility_id, into = c("original_plan_id", "utility_id"), sep = "-") -> lbp 
                
                
                
                til.i <- top.impacts.leads[k,]
                ticr.i <- top.impacts.cr[k,]
                
                if(between(til.i$Average_Relative_Effect, ticr.i$Avg_Rel_Eff_Lo_90_Conf_Interval, ticr.i$Avg_Rel_Eff_Up_90_Conf_Interval)){
                    
                    
                    leads.ct[leads.ct$utility_name_id == top.impacts.leads[k, "utility_name_id"],] %>%
                        filter(product == top.impacts.leads[k, "product"]) %>%
                        .[,!names(.) %in% c("utility_name_id", "product")] -> leads.i
                    
                    lbp %>%
                        filter(utility_id == top.impacts.leads[k, "utility_id"]) %>%
                        .[,!names(.) %in% c("utility_id")] %>%
                        spread(key = original_plan_id, value = n, fill = 0) -> lbp.i
                    
                    for.cor.i <- merge(leads.i, lbp.i, by = "date_submitted")
                    
                    arrange(reshape2::melt(cor(for.cor.i$n, for.cor.i[,-1:-2])),desc(value)) %>%
                        .[,!names(.) %in% c("Var1")] %>%
                        dplyr::rename(original_plan_id = Var2, cor = value) -> cor.i
                    
                    lbp.i %>%
                        .[,names(.) %in% c("date_submitted", cor.i$original_plan_id[1:5])] %>%
                        reshape2::melt("date_submitted") %>%
                        dplyr::rename(original_plan_id = variable, n = value) -> top.lbp
                    
                    changes %>%
                        filter(utility_name_id == top.impacts.leads[k, "utility_name_id"]) %>%
                        filter(original_plan_id == as.numeric(cor.i$original_plan_id[1])) -> changes.i
                    
                    changes.i <- arrange(changes.i[!duplicated(changes.i),], plan_audit_id)
                    
                    if(dim(changes.i)[1] == 0){
                        
                        paste0(" ")
                        
                    } else {
                        if(between(changes.i[1,"date"], change.date.i - 3, change.date.i + 3)){
                            
                            new_plan_status = changes.i[changes.i$plan_audit_id == max(changes.i$plan_audit_id), "plan_status_name"]
                            
                            new_plan_status
                            
                        } else {
                            paste0(" ")
                        }
                        
                    }
                    
                } else{
                    paste0(" ")
                }
                
            })
            
            output$plan_id6.tab <- renderText({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                leads <- get.causal.impact.data()[[7]]
                
                
                k <- 6
                
                change.date.i <- top.impacts.leads[k, "graph.date"]
                
                leads %>%
                    mutate(n = 1) %>%
                    unite(original_plan_plus_utility_id, original_plan_id, utility_id, sep = "-") %>%
                    aggregate(n~original_plan_plus_utility_id + date_submitted, data=., sum) -> leads.by.plan_plus_utility_id     
                
                lbp.spread <- spread(leads.by.plan_plus_utility_id,key = original_plan_plus_utility_id, value = n, fill = 0)
                
                lbp.zoo <- zoo(lbp.spread[,-1], lbp.spread[,1])
                
                merge(lbp.zoo, zoo(1, seq(today() - daysback, today(), by = 1)), all = TRUE) %>% 
                    .[,-(ncol(.))] %>%
                    fortify.zoo(.) %>%
                    reshape2::melt("Index") %>%
                    dplyr::rename(date_submitted = Index, original_plan_plus_utility_id = variable, n = value) %>%
                    separate(original_plan_plus_utility_id, into = c("original_plan_id", "utility_id"), sep = "-") -> lbp 
                
                
                
                til.i <- top.impacts.leads[k,]
                ticr.i <- top.impacts.cr[k,]
                
                if(between(til.i$Average_Relative_Effect, ticr.i$Avg_Rel_Eff_Lo_90_Conf_Interval, ticr.i$Avg_Rel_Eff_Up_90_Conf_Interval)){
                    
                    
                    leads.ct[leads.ct$utility_name_id == top.impacts.leads[k, "utility_name_id"],] %>%
                        filter(product == top.impacts.leads[k, "product"]) %>%
                        .[,!names(.) %in% c("utility_name_id", "product")] -> leads.i
                    
                    lbp %>%
                        filter(utility_id == top.impacts.leads[k, "utility_id"]) %>%
                        .[,!names(.) %in% c("utility_id")] %>%
                        spread(key = original_plan_id, value = n, fill = 0) -> lbp.i
                    
                    for.cor.i <- merge(leads.i, lbp.i, by = "date_submitted")
                    
                    arrange(reshape2::melt(cor(for.cor.i$n, for.cor.i[,-1:-2])),desc(value)) %>%
                        .[,!names(.) %in% c("Var1")] %>%
                        dplyr::rename(original_plan_id = Var2, cor = value) -> cor.i
                    
                    lbp.i %>%
                        .[,names(.) %in% c("date_submitted", cor.i$original_plan_id[1:5])] %>%
                        reshape2::melt("date_submitted") %>%
                        dplyr::rename(original_plan_id = variable, n = value) -> top.lbp
                    
                    changes %>%
                        filter(utility_name_id == top.impacts.leads[k, "utility_name_id"]) %>%
                        filter(original_plan_id == as.numeric(cor.i$original_plan_id[1])) -> changes.i
                    
                    changes.i <- arrange(changes.i[!duplicated(changes.i),], plan_audit_id)
                    
                    
                    if(dim(changes.i)[1] == 0){
                        
                        paste0(" ")
                        
                    } else {
                        if(between(changes.i[1,"date"], change.date.i - 3, change.date.i + 3)){
                            
                            as.character(paste("Plan ID: ", changes.i[1, c("original_plan_id")], sep = ""))
                            
                        } else {
                            paste0(" ")
                        }
                        
                    }
                    
                } else{
                    paste0(" ")
                }
                
                
            })
            
            
            
            output$price_change6 <- renderText({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                leads <- get.causal.impact.data()[[7]]
                
                
                k <- 6
                
                change.date.i <- top.impacts.leads[k, "graph.date"]
                
                leads %>%
                    mutate(n = 1) %>%
                    unite(original_plan_plus_utility_id, original_plan_id, utility_id, sep = "-") %>%
                    aggregate(n~original_plan_plus_utility_id + date_submitted, data=., sum) -> leads.by.plan_plus_utility_id     
                
                lbp.spread <- spread(leads.by.plan_plus_utility_id,key = original_plan_plus_utility_id, value = n, fill = 0)
                
                lbp.zoo <- zoo(lbp.spread[,-1], lbp.spread[,1])
                
                merge(lbp.zoo, zoo(1, seq(today() - daysback, today(), by = 1)), all = TRUE) %>% 
                    .[,-(ncol(.))] %>%
                    fortify.zoo(.) %>%
                    reshape2::melt("Index") %>%
                    dplyr::rename(date_submitted = Index, original_plan_plus_utility_id = variable, n = value) %>%
                    separate(original_plan_plus_utility_id, into = c("original_plan_id", "utility_id"), sep = "-") -> lbp 
                
                
                
                til.i <- top.impacts.leads[k,]
                ticr.i <- top.impacts.cr[k,]
                
                if(between(til.i$Average_Relative_Effect, ticr.i$Avg_Rel_Eff_Lo_90_Conf_Interval, ticr.i$Avg_Rel_Eff_Up_90_Conf_Interval)){
                    
                    
                    leads.ct[leads.ct$utility_name_id == top.impacts.leads[k, "utility_name_id"],] %>%
                        filter(product == top.impacts.leads[k, "product"]) %>%
                        .[,!names(.) %in% c("utility_name_id", "product")] -> leads.i
                    
                    lbp %>%
                        filter(utility_id == top.impacts.leads[k, "utility_id"]) %>%
                        .[,!names(.) %in% c("utility_id")] %>%
                        spread(key = original_plan_id, value = n, fill = 0) -> lbp.i
                    
                    for.cor.i <- merge(leads.i, lbp.i, by = "date_submitted")
                    
                    arrange(reshape2::melt(cor(for.cor.i$n, for.cor.i[,-1:-2])),desc(value)) %>%
                        .[,!names(.) %in% c("Var1")] %>%
                        dplyr::rename(original_plan_id = Var2, cor = value) -> cor.i
                    
                    lbp.i %>%
                        .[,names(.) %in% c("date_submitted", cor.i$original_plan_id[1:5])] %>%
                        reshape2::melt("date_submitted") %>%
                        dplyr::rename(original_plan_id = variable, n = value) -> top.lbp
                    
                    changes %>%
                        filter(utility_name_id == top.impacts.leads[k, "utility_name_id"]) %>%
                        filter(original_plan_id == as.numeric(cor.i$original_plan_id[1])) -> changes.i
                    
                    changes.i <- arrange(changes.i[!duplicated(changes.i),], plan_audit_id)
                    
                    
                    
                    if(dim(changes.i)[1] == 0){
                        
                        paste0(" ")
                        
                    } else {
                        if(between(changes.i[1,"date"], change.date.i - 3, change.date.i + 3)){
                            
                            old_price = changes.i[changes.i$plan_audit_id == min(changes.i$plan_audit_id), "plan_price"]
                            new_price = changes.i[changes.i$plan_audit_id == max(changes.i$plan_audit_id), "plan_price"]
                            
                            paste(old_price, "¢", " -> ", new_price, "¢", sep = "")
                            
                        } else {
                            paste0(" ")
                        }
                        
                    }
                    
                } else{
                    paste0(" ")
                }
                
                
            })
            
            
            
            
            
            
            output$change.6 <- renderInfoBox({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                leads <- get.causal.impact.data()[[7]]
                
                k <- 6
                
                change.date.i <- max(changes[changes$change_index == top.impacts.leads$change_index[k], "date"])
                
                product.i <- unique(changes[changes$change_index == top.impacts.leads$change_index[k], "product"])
                utility.i <- paste(unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility"]), unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility_id"]), sep = " - ")
                
                leads.ct %>%
                    filter(utility_name_id == utility.i) %>%
                    filter(product == product.i) -> leads.i
                
                leads.ts <- zoo(leads.i[,!names(leads.i) %in% c("date_submitted", "utility_name_id", "product")], order.by = leads.i$date_submitted)
                pre.i <- c(min(leads.i$date_submitted), change.date.i)
                post.i <- c(change.date.i + 1, max(leads.i$date_submitted))
                impact.i <- try.Causal.Impact(time_series = leads.ts, pre_date = pre.i, post_date = post.i, conf_int = 0.05)
                
                if(impact.i$summary$AbsEffect[1] < 0){
                    subt <- "Lead Volume Decline"
                    ico <- icon("arrow-down")
                } else {
                    subt <- "Lead Volume Increase"
                    ico <- icon("arrow-up")
                }
                
                infoBox(
                    "What occurred:", subt, icon = ico,
                    color = "purple"
                )
                
            })
            
            output$utility.6 <- renderInfoBox({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                
                k <- 6
                infoBox(
                    "Where:", paste(top.impacts.leads[k, c("state")], top.impacts.leads[k, c("utility")], sep = " - "), icon = icon("plug", "fa-0.5x"),
                    color = "green"
                )        
            })
            
            output$product.6 <- renderInfoBox({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                
                k <- 6
                if(top.impacts.leads[k, c("product")] == "Electricity"){
                    ico <- icon("bolt", "fa-0.5x")
                } else{
                    ico <- icon("fire", "fa-0.5x")
                }
                infoBox(
                    "Which product:", top.impacts.leads[k, c("product")], icon = ico,
                    color = "yellow"
                )        
            })
            
            
            output$causal.impact.cr.plot6 <- renderPlot({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                
                changes.unique <- changes[!duplicated(changes[,c("date", "utility_id")]),]
                unique.index <- unique(changes.unique$change_index)
                
                k <- 6
                
                change.date.i <- max(changes[changes$change_index == top.impacts.leads$change_index[k], "date"])
                
                product.i <- unique(changes[changes$change_index == top.impacts.leads$change_index[k], "product"])
                utility.i <- paste(unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility"]), unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility_id"]), sep = " - ")
                
                cr.ct %>%
                    filter(utility_name_id == utility.i) %>%
                    filter(product == product.i) -> leads.i
                
                leads.ts <- zoo(leads.i[,!names(leads.i) %in% c("date_submitted", "utility_name_id", "product")], order.by = leads.i$date_submitted)
                pre.i <- c(min(leads.i$date_submitted), change.date.i)
                post.i <- c(change.date.i + 1, max(leads.i$date_submitted))
                impact.i <- try.Causal.Impact(time_series = leads.ts, pre_date = pre.i, post_date = post.i, conf_int = 0.05)
                plot(impact.i)
            })
            
            
            output$causal.impact.cr.summary.EXCLUDE <- renderPrint({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                
                changes.unique <- changes[!duplicated(changes[,c("date", "utility_id")]),]
                unique.index <- unique(changes.unique$change_index)
                
                k <- 6
                
                change.date.i <- max(changes[changes$change_index == top.impacts.leads$change_index[k], "date"])
                
                product.i <- unique(changes[changes$change_index == top.impacts.leads$change_index[k], "product"])
                utility.i <- paste(unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility"]), unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility_id"]), sep = " - ")
                
                cr.ct %>%
                    filter(utility_name_id == utility.i) %>%
                    filter(product == product.i) -> leads.i
                
                leads.ts <- zoo(leads.i[,!names(leads.i) %in% c("date_submitted", "utility_name_id", "product")], order.by = leads.i$date_submitted)
                pre.i <- c(min(leads.i$date_submitted), change.date.i)
                post.i <- c(change.date.i + 1, max(leads.i$date_submitted))
                impact.i <- try.Causal.Impact(time_series = leads.ts, pre_date = pre.i, post_date = post.i, conf_int = 0.05)
                summary(impact.i)
            })        
            
            
            output$causal.impact.cr.summary6 <- renderPrint({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                
                changes.unique <- changes[!duplicated(changes[,c("date", "utility_id")]),]
                unique.index <- unique(changes.unique$change_index)
                
                k <- 6
                
                change.date.i <- max(changes[changes$change_index == top.impacts.leads$change_index[k], "date"])
                
                product.i <- unique(changes[changes$change_index == top.impacts.leads$change_index[k], "product"])
                utility.i <- paste(unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility"]), unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility_id"]), sep = " - ")
                cr.ct %>%
                    filter(utility_name_id == utility.i) %>%
                    filter(product == product.i) -> leads.i
                
                leads.ts <- zoo(leads.i[,!names(leads.i) %in% c("date_submitted", "utility_name_id", "product")], order.by = leads.i$date_submitted)
                pre.i <- c(min(leads.i$date_submitted), change.date.i)
                post.i <- c(change.date.i + 1, max(leads.i$date_submitted))
                impact.i <- try.Causal.Impact(time_series = leads.ts, pre_date = pre.i, post_date = post.i, conf_int = 0.05)
                summary(impact.i)
            })
            
            output$causal.impact.leads.summary6 <- renderPrint({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                
                changes.unique <- changes[!duplicated(changes[,c("date", "utility_id")]),]
                unique.index <- unique(changes.unique$change_index)
                
                k <- 6
                
                change.date.i <- max(changes[changes$change_index == top.impacts.leads$change_index[k], "date"])
                
                product.i <- unique(changes[changes$change_index == top.impacts.leads$change_index[k], "product"])
                utility.i <- paste(unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility"]), unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility_id"]), sep = " - ")
                leads.ct %>%
                    filter(utility_name_id == utility.i) %>%
                    filter(product == product.i) -> leads.i
                
                leads.ts <- zoo(leads.i[,!names(leads.i) %in% c("date_submitted", "utility_name_id", "product")], order.by = leads.i$date_submitted)
                pre.i <- c(min(leads.i$date_submitted), change.date.i)
                post.i <- c(change.date.i + 1, max(leads.i$date_submitted))
                impact.i <- try.Causal.Impact(time_series = leads.ts, pre_date = pre.i, post_date = post.i, conf_int = 0.05)
                summary(impact.i)
            })
            
            
            output$causal.impact.leads.plot6 <- renderPlot({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                
                changes.unique <- changes[!duplicated(changes[,c("date", "utility_id")]),]
                unique.index <- unique(changes.unique$change_index)
                
                k <- 6
                
                change.date.i <- max(changes[changes$change_index == top.impacts.leads$change_index[k], "date"])
                
                product.i <- unique(changes[changes$change_index == top.impacts.leads$change_index[k], "product"])
                utility.i <- paste(unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility"]), unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility_id"]), sep = " - ")
                leads.ct %>%
                    filter(utility_name_id == utility.i) %>%
                    filter(product == product.i) -> leads.i
                
                leads.ts <- zoo(leads.i[,!names(leads.i) %in% c("date_submitted", "utility_name_id", "product")], order.by = leads.i$date_submitted)
                pre.i <- c(min(leads.i$date_submitted), change.date.i)
                post.i <- c(change.date.i + 1, max(leads.i$date_submitted))
                impact.i <- try.Causal.Impact(time_series = leads.ts, pre_date = pre.i, post_date = post.i, conf_int = 0.05)
                plot(impact.i)
            })
            
            
            output$complete.plan.changes6 <- renderPlotly({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                
                changes.unique <- changes[!duplicated(changes[,c("date", "utility_id")]),]
                unique.index <- unique(changes.unique$change_index)
                
                k <- 6
                
                change.date.i <- max(changes[changes$change_index == top.impacts.leads$change_index[k], "date"])
                
                product.i <- unique(changes[changes$change_index == top.impacts.leads$change_index[k], "product"])
                utility.i <- paste(unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility"]), unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility_id"]), sep = " - ")
                
                utility.name.i <- unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility_name_id"])
                changes[changes$utility_name_id == utility.name.i, ] %>%
                    filter(graph.date > (today()-(daysback+1))) %>%
                    mutate(Supplier_Plan = paste(change, supplier, plan_name, sep = " - ")) -> changes.full.i
                
                daily.prices %>%
                    filter(utility_name_id == utility.i) %>%
                    filter(product == product.i) -> daily.prices.i
                
                
                
                p <- ggplot() + 
                    geom_line(data = daily.prices.i, aes(date, plan_price.mean)) +
                    geom_line(data = daily.prices.i, aes(date, plan_price.max)) +
                    geom_line(data = daily.prices.i, aes(date, plan_price.min)) +
                    geom_line(data = changes.full.i[changes.full.i$change == "plan_change",], aes(graph.date, plan_price, col = Supplier_Plan)) +
                    geom_point(data = changes.full.i[changes.full.i$change == "plan_end",], aes(graph.date, plan_price, col = Supplier_Plan)) + 
                    geom_point(data = changes.full.i[changes.full.i$change == "new_plan",], aes(graph.date, plan_price, col = Supplier_Plan)) +
                    geom_vline(xintercept = as.numeric(change.date.i), linetype = "longdash") +
                    ggtitle(paste("All Plan Changes Around", change.date.i, sep = " ")) +
                    xlab("Date") +
                    ylab("Plan Price (cents)") +
                    theme(legend.position = "none", panel.background=element_blank(), panel.grid.major = element_line(colour = "grey"))
                
                ggplotly(p)
            })
            
            
            
            
            
            output$plan_breakdown6 <- renderPlot({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                leads <- get.causal.impact.data()[[7]]
                sessions.ct <- get.causal.impact.data()[[8]]     
                plan.inv <- get.causal.impact.data()[[9]]
                
                
                changes.unique <- changes[!duplicated(changes[,c("date", "utility_id")]),]
                unique.index <- unique(changes.unique$change_index)
                
                k <- 6
                
                change.date.i <- max(changes[changes$change_index == top.impacts.leads$change_index[k], "date"])
                
                product.i <- unique(changes[changes$change_index == top.impacts.leads$change_index[k], "product"])
                utility.i <- paste(unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility"]), unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility_id"]), sep = " - ")
                
                utility.name.i <- unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility_name_id"])
                changes[changes$utility_name_id == utility.name.i, ] %>%
                    filter(graph.date > (today()-(daysback+1))) %>%
                    mutate(suplan = paste(change, supplier, plan_name, sep = " - ")) -> changes.full.i
                
                daily.prices %>%
                    filter(utility_name_id == utility.i) %>%
                    filter(product == product.i) -> daily.prices.i
                
                
                leads %>%
                    mutate(n = 1) %>%
                    unite(original_plan_plus_utility_id, original_plan_id, utility_id, sep = "-") %>%
                    aggregate(n~original_plan_plus_utility_id + date_submitted, data=., sum) -> leads.by.plan_plus_utility_id     
                
                lbp.spread <- spread(leads.by.plan_plus_utility_id,key = original_plan_plus_utility_id, value = n, fill = 0)
                
                lbp.zoo <- zoo(lbp.spread[,-1], lbp.spread[,1])
                
                merge(lbp.zoo, zoo(1, seq(today() - daysback, today(), by = 1)), all = TRUE) %>% 
                    .[,-(ncol(.))] %>%
                    fortify.zoo(.) %>%
                    reshape2::melt("Index") %>%
                    dplyr::rename(date_submitted = Index, original_plan_plus_utility_id = variable, n = value) %>%
                    separate(original_plan_plus_utility_id, into = c("original_plan_id", "utility_id"), sep = "-") -> lbp 
                
                
                
                til.i <- top.impacts.leads[k,]
                ticr.i <- top.impacts.cr[k,]
                
                if(between(til.i$Average_Relative_Effect, ticr.i$Avg_Rel_Eff_Lo_90_Conf_Interval, ticr.i$Avg_Rel_Eff_Up_90_Conf_Interval)){
                    
                    
                    leads.ct[leads.ct$utility_name_id == top.impacts.leads[k, "utility_name_id"],] %>%
                        filter(product == top.impacts.leads[k, "product"]) %>%
                        .[,!names(.) %in% c("utility_name_id", "product")] -> leads.i
                    
                    lbp %>%
                        filter(utility_id == top.impacts.leads[k, "utility_id"]) %>%
                        .[,!names(.) %in% c("utility_id")] %>%
                        spread(key = original_plan_id, value = n, fill = 0) -> lbp.i
                    
                    for.cor.i <- merge(leads.i, lbp.i, by = "date_submitted")
                    
                    arrange(reshape2::melt(cor(for.cor.i$n, for.cor.i[,-1:-2])),desc(value)) %>%
                        .[,!names(.) %in% c("Var1")] %>%
                        dplyr::rename(original_plan_id = Var2, cor = value) -> cor.i
                    
                    lbp.i %>%
                        .[,names(.) %in% c("date_submitted", cor.i$original_plan_id[1:5])] %>%
                        reshape2::melt("date_submitted") %>%
                        dplyr::rename(original_plan_id = variable, n = value) %>%
                        merge(unique(plan.inv[,c("supplier", "plan_name", "original_plan_id")]), by = "original_plan_id", all.x = TRUE) %>%
                        unite(suplan, supplier, plan_name, sep = " - ", remove = TRUE) %>%
                        .[,!names(.) %in% c("original_plan_id")] -> top.lbp
                    
                    changes %>%
                        filter(utility_name_id == top.impacts.leads[k, "utility_name_id"]) %>%
                        filter(original_plan_id == as.numeric(cor.i$original_plan_id[1])) -> changes.i
                    
                    changes.i <- arrange(changes.i[!duplicated(changes.i),], plan_audit_id)
                    
                    leads.i %>%
                        mutate(suplan = "Total Leads") %>%
                        .[,c("date_submitted", "n", "suplan")] %>%
                        rbind(top.lbp) -> leads.for.plot
                    
                    leads.for.plot$suplan <- as.character(leads.for.plot$suplan)
                    leads.for.plot$suplan <- factor(leads.for.plot$suplan, levels = c("Total Leads", unique(top.lbp$suplan)))
                    
                    names(leads.for.plot) <- c("date_submitted", "n", "suplan")
                    
                    ggplot(leads.for.plot, aes(date_submitted, n)) + 
                        geom_line() + 
                        facet_wrap(~suplan, ncol = 1) + 
                        ggtitle("Lead Volume of Top Correlating Plans") +
                        theme(legend.position = "none", panel.background=element_blank(), panel.grid.major = element_line(colour = "grey"), strip.text.y = element_text(angle=0), plot.title = element_text(hjust = 0.5, size = 18))+
                        xlab("Date") +
                        ylab("Lead Volume") +
                        geom_vline(xintercept = as.numeric(change.date.i), linetype = "longdash")
                    
                } else{
                    sessions.ct %>%
                        filter(utility_name_id == top.impacts.leads[k, "utility_name_id"]) %>%
                        aggregate(uniquePageviews ~ date_submitted, data=., sum) -> sessions.for.plot
                    
                    ggplot(sessions.for.plot, aes(date_submitted, uniquePageviews)) + 
                        geom_line() + 
                        ggtitle(paste("Sessions in ", top.impacts.leads[k, "utility"], sep = "")) +
                        theme(legend.position = "none", panel.background=element_blank(), panel.grid.major = element_line(colour = "grey")) +
                        xlab("Date") +
                        ylab("Sessions") +
                        geom_vline(xintercept = as.numeric(change.date.i), linetype = "longdash") +
                        theme(plot.title = element_text(hjust = 0.5, size = 18)) +
                        stat_smooth(method=loess)
                    
                }
                
                
            })
            
            
            # # # --- # # # --- # # # --- # # # --- # # # ---
            
            #                      TAB 7                    #  
            
            # # # --- # # # --- # # # --- # # # --- # # # ---
            
            
            
            
            
            
            
            output$likelihood.description.7 <- renderText({
                
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                leads <- get.causal.impact.data()[[7]]
                
                
                k <- 7
                
                change.date.i <- top.impacts.leads[k, "graph.date"]
                
                leads %>%
                    mutate(n = 1) %>%
                    unite(original_plan_plus_utility_id, original_plan_id, utility_id, sep = "-") %>%
                    aggregate(n~original_plan_plus_utility_id + date_submitted, data=., sum) -> leads.by.plan_plus_utility_id     
                
                lbp.spread <- spread(leads.by.plan_plus_utility_id,key = original_plan_plus_utility_id, value = n, fill = 0)
                
                lbp.zoo <- zoo(lbp.spread[,-1], lbp.spread[,1])
                
                merge(lbp.zoo, zoo(1, seq(today() - daysback, today(), by = 1)), all = TRUE) %>% 
                    .[,-(ncol(.))] %>%
                    fortify.zoo(.) %>%
                    reshape2::melt("Index") %>%
                    dplyr::rename(date_submitted = Index, original_plan_plus_utility_id = variable, n = value) %>%
                    separate(original_plan_plus_utility_id, into = c("original_plan_id", "utility_id"), sep = "-") -> lbp 
                
                
                til.i <- top.impacts.leads[k,]
                ticr.i <- top.impacts.cr[k,]
                
                if(between(til.i$Average_Relative_Effect, ticr.i$Avg_Rel_Eff_Lo_90_Conf_Interval, ticr.i$Avg_Rel_Eff_Up_90_Conf_Interval)){
                    
                    
                    leads.ct[leads.ct$utility_name_id == top.impacts.leads[k, "utility_name_id"],] %>%
                        filter(product == top.impacts.leads[k, "product"]) %>%
                        .[,!names(.) %in% c("utility_name_id", "product")] -> leads.i
                    
                    lbp %>%
                        filter(utility_id == top.impacts.leads[k, "utility_id"]) %>%
                        .[,!names(.) %in% c("utility_id")] %>%
                        spread(key = original_plan_id, value = n, fill = 0) -> lbp.i
                    
                    for.cor.i <- merge(leads.i, lbp.i, by = "date_submitted")
                    
                    arrange(reshape2::melt(cor(for.cor.i$n, for.cor.i[,-1:-2])),desc(value)) %>%
                        .[,!names(.) %in% c("Var1")] %>%
                        dplyr::rename(original_plan_id = Var2, cor = value) -> cor.i
                    
                    lbp.i %>%
                        .[,names(.) %in% c("date_submitted", cor.i$original_plan_id[1:5])] %>%
                        reshape2::melt("date_submitted") %>%
                        dplyr::rename(original_plan_id = variable, n = value) -> top.lbp
                    
                    changes %>%
                        filter(utility_name_id == top.impacts.leads[k, "utility_name_id"]) %>%
                        filter(original_plan_id == as.numeric(cor.i$original_plan_id[1])) -> changes.i
                    
                    changes.i <- arrange(changes.i[!duplicated(changes.i),], plan_audit_id)
                    
                    if(dim(changes.i)[1] == 0){
                        
                        description <- paste0("Both lead volume and conversion rate were down, although no particular plan changes occurred that could be traced back to. See Lead Volume of Top Correlating Plans below for possibile causes.")
                        
                    } else {
                        if(between(changes.i[1,"date"], change.date.i - 3, change.date.i + 3)){
                            
                            description <- paste0("This plan change is highly likely to have caused the observed change in lead and conversion rate.")
                            
                        } else {
                            description <- paste0("Both lead volume and conversion rate were down, although no particular plan changes occurred that could be traced back to. See Lead Volume of Top Correlating Plans below for possibile causes.")
                        }
                    }
                    
                } else{
                    description <- paste0("No plan changes were found relating to this impact, and conversion rate either stayed relatively the same or went up while leads went down. Therefore the impact can be traced back to a change in traffic in this utility zone.")
                }
                
                description
                
            })
            
            
            
            
            
            output$plan_name7.tab <- renderText({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                leads <- get.causal.impact.data()[[7]]
                
                
                k <- 7
                
                change.date.i <- top.impacts.leads[k, "graph.date"]
                
                leads %>%
                    mutate(n = 1) %>%
                    unite(original_plan_plus_utility_id, original_plan_id, utility_id, sep = "-") %>%
                    aggregate(n~original_plan_plus_utility_id + date_submitted, data=., sum) -> leads.by.plan_plus_utility_id     
                
                lbp.spread <- spread(leads.by.plan_plus_utility_id,key = original_plan_plus_utility_id, value = n, fill = 0)
                
                lbp.zoo <- zoo(lbp.spread[,-1], lbp.spread[,1])
                
                merge(lbp.zoo, zoo(1, seq(today() - daysback, today(), by = 1)), all = TRUE) %>% 
                    .[,-(ncol(.))] %>%
                    fortify.zoo(.) %>%
                    reshape2::melt("Index") %>%
                    dplyr::rename(date_submitted = Index, original_plan_plus_utility_id = variable, n = value) %>%
                    separate(original_plan_plus_utility_id, into = c("original_plan_id", "utility_id"), sep = "-") -> lbp 
                
                
                
                
                
                til.i <- top.impacts.leads[k,]
                ticr.i <- top.impacts.cr[k,]
                
                if(between(til.i$Average_Relative_Effect, ticr.i$Avg_Rel_Eff_Lo_90_Conf_Interval, ticr.i$Avg_Rel_Eff_Up_90_Conf_Interval)){
                    
                    
                    leads.ct[leads.ct$utility_name_id == top.impacts.leads[k, "utility_name_id"],] %>%
                        filter(product == top.impacts.leads[k, "product"]) %>%
                        .[,!names(.) %in% c("utility_name_id", "product")] -> leads.i
                    
                    lbp %>%
                        filter(utility_id == top.impacts.leads[k, "utility_id"]) %>%
                        .[,!names(.) %in% c("utility_id")] %>%
                        spread(key = original_plan_id, value = n, fill = 0) -> lbp.i
                    
                    for.cor.i <- merge(leads.i, lbp.i, by = "date_submitted")
                    
                    arrange(reshape2::melt(cor(for.cor.i$n, for.cor.i[,-1:-2])),desc(value)) %>%
                        .[,!names(.) %in% c("Var1")] %>%
                        dplyr::rename(original_plan_id = Var2, cor = value) -> cor.i
                    
                    lbp.i %>%
                        .[,names(.) %in% c("date_submitted", cor.i$original_plan_id[1:5])] %>%
                        reshape2::melt("date_submitted") %>%
                        dplyr::rename(original_plan_id = variable, n = value) -> top.lbp
                    
                    changes %>%
                        filter(utility_name_id == top.impacts.leads[k, "utility_name_id"]) %>%
                        filter(original_plan_id == as.numeric(cor.i$original_plan_id[1])) -> changes.i
                    
                    changes.i <- arrange(changes.i[!duplicated(changes.i),], plan_audit_id)
                    
                    if(dim(changes.i)[1] == 0){
                        
                        paste0("Multiple Possibilities")
                        
                    } else {
                        if(between(changes.i[1,"date"], change.date.i - 3, change.date.i + 3)){
                            
                            as.character(changes.i[1, c("plan_name")])
                            
                        } else {
                            paste0("Multiple Possibilities")
                        }
                        
                    }
                    
                } else{
                    paste0("Traffic Change")
                }
                
                
                
            })
            
            
            output$plan_term7.tab <- renderText({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                leads <- get.causal.impact.data()[[7]]
                
                
                k <- 7
                
                change.date.i <- top.impacts.leads[k, "graph.date"]
                
                leads %>%
                    mutate(n = 1) %>%
                    unite(original_plan_plus_utility_id, original_plan_id, utility_id, sep = "-") %>%
                    aggregate(n~original_plan_plus_utility_id + date_submitted, data=., sum) -> leads.by.plan_plus_utility_id     
                
                lbp.spread <- spread(leads.by.plan_plus_utility_id,key = original_plan_plus_utility_id, value = n, fill = 0)
                
                lbp.zoo <- zoo(lbp.spread[,-1], lbp.spread[,1])
                
                merge(lbp.zoo, zoo(1, seq(today() - daysback, today(), by = 1)), all = TRUE) %>% 
                    .[,-(ncol(.))] %>%
                    fortify.zoo(.) %>%
                    reshape2::melt("Index") %>%
                    dplyr::rename(date_submitted = Index, original_plan_plus_utility_id = variable, n = value) %>%
                    separate(original_plan_plus_utility_id, into = c("original_plan_id", "utility_id"), sep = "-") -> lbp 
                
                
                
                til.i <- top.impacts.leads[k,]
                ticr.i <- top.impacts.cr[k,]
                
                if(between(til.i$Average_Relative_Effect, ticr.i$Avg_Rel_Eff_Lo_90_Conf_Interval, ticr.i$Avg_Rel_Eff_Up_90_Conf_Interval)){
                    
                    
                    leads.ct[leads.ct$utility_name_id == top.impacts.leads[k, "utility_name_id"],] %>%
                        filter(product == top.impacts.leads[k, "product"]) %>%
                        .[,!names(.) %in% c("utility_name_id", "product")] -> leads.i
                    
                    lbp %>%
                        filter(utility_id == top.impacts.leads[k, "utility_id"]) %>%
                        .[,!names(.) %in% c("utility_id")] %>%
                        spread(key = original_plan_id, value = n, fill = 0) -> lbp.i
                    
                    for.cor.i <- merge(leads.i, lbp.i, by = "date_submitted")
                    
                    arrange(reshape2::melt(cor(for.cor.i$n, for.cor.i[,-1:-2])),desc(value)) %>%
                        .[,!names(.) %in% c("Var1")] %>%
                        dplyr::rename(original_plan_id = Var2, cor = value) -> cor.i
                    
                    lbp.i %>%
                        .[,names(.) %in% c("date_submitted", cor.i$original_plan_id[1:5])] %>%
                        reshape2::melt("date_submitted") %>%
                        dplyr::rename(original_plan_id = variable, n = value) -> top.lbp
                    
                    changes %>%
                        filter(utility_name_id == top.impacts.leads[k, "utility_name_id"]) %>%
                        filter(original_plan_id == as.numeric(cor.i$original_plan_id[1])) -> changes.i
                    
                    changes.i <- arrange(changes.i[!duplicated(changes.i),], plan_audit_id)
                    
                    if(dim(changes.i)[1] == 0){
                        
                        paste0(" ")
                        
                    } else {
                        if(between(changes.i[1,"date"], change.date.i - 3, change.date.i + 3)){
                            
                            as.character(paste(changes.i[1, c("contract_term")], " Months ", changes.i[1, c("contract_type")], sep = ""))
                            
                        } else {
                            paste0(" ")
                        }
                        
                    }
                    
                } else{
                    paste0(" ")
                }
                
            }) 
            
            
            output$plan_supplier7.tab <- renderText({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                leads <- get.causal.impact.data()[[7]]
                
                
                k <- 7
                
                change.date.i <- top.impacts.leads[k, "graph.date"]
                
                leads %>%
                    mutate(n = 1) %>%
                    unite(original_plan_plus_utility_id, original_plan_id, utility_id, sep = "-") %>%
                    aggregate(n~original_plan_plus_utility_id + date_submitted, data=., sum) -> leads.by.plan_plus_utility_id     
                
                lbp.spread <- spread(leads.by.plan_plus_utility_id,key = original_plan_plus_utility_id, value = n, fill = 0)
                
                lbp.zoo <- zoo(lbp.spread[,-1], lbp.spread[,1])
                
                merge(lbp.zoo, zoo(1, seq(today() - daysback, today(), by = 1)), all = TRUE) %>% 
                    .[,-(ncol(.))] %>%
                    fortify.zoo(.) %>%
                    reshape2::melt("Index") %>%
                    dplyr::rename(date_submitted = Index, original_plan_plus_utility_id = variable, n = value) %>%
                    separate(original_plan_plus_utility_id, into = c("original_plan_id", "utility_id"), sep = "-") -> lbp 
                
                
                
                til.i <- top.impacts.leads[k,]
                ticr.i <- top.impacts.cr[k,]
                
                if(between(til.i$Average_Relative_Effect, ticr.i$Avg_Rel_Eff_Lo_90_Conf_Interval, ticr.i$Avg_Rel_Eff_Up_90_Conf_Interval)){
                    
                    
                    leads.ct[leads.ct$utility_name_id == top.impacts.leads[k, "utility_name_id"],] %>%
                        filter(product == top.impacts.leads[k, "product"]) %>%
                        .[,!names(.) %in% c("utility_name_id", "product")] -> leads.i
                    
                    lbp %>%
                        filter(utility_id == top.impacts.leads[k, "utility_id"]) %>%
                        .[,!names(.) %in% c("utility_id")] %>%
                        spread(key = original_plan_id, value = n, fill = 0) -> lbp.i
                    
                    for.cor.i <- merge(leads.i, lbp.i, by = "date_submitted")
                    
                    arrange(reshape2::melt(cor(for.cor.i$n, for.cor.i[,-1:-2])),desc(value)) %>%
                        .[,!names(.) %in% c("Var1")] %>%
                        dplyr::rename(original_plan_id = Var2, cor = value) -> cor.i
                    
                    lbp.i %>%
                        .[,names(.) %in% c("date_submitted", cor.i$original_plan_id[1:5])] %>%
                        reshape2::melt("date_submitted") %>%
                        dplyr::rename(original_plan_id = variable, n = value) -> top.lbp
                    
                    changes %>%
                        filter(utility_name_id == top.impacts.leads[k, "utility_name_id"]) %>%
                        filter(original_plan_id == as.numeric(cor.i$original_plan_id[1])) -> changes.i
                    
                    changes.i <- arrange(changes.i[!duplicated(changes.i),], plan_audit_id)
                    
                    if(dim(changes.i)[1] == 0){
                        
                        paste0(" ")
                        
                    } else {
                        if(between(changes.i[1,"date"], change.date.i - 3, change.date.i + 3)){
                            
                            as.character(changes.i[1, c("supplier")])
                            
                        } else {
                            paste0(" ")
                        }
                        
                    }
                    
                } else{
                    paste0(" ")
                }
                
            })
            
            
            output$plan_active7.tab <- renderText({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                leads <- get.causal.impact.data()[[7]]
                
                
                k <- 7
                
                change.date.i <- top.impacts.leads[k, "graph.date"]
                
                leads %>%
                    mutate(n = 1) %>%
                    unite(original_plan_plus_utility_id, original_plan_id, utility_id, sep = "-") %>%
                    aggregate(n~original_plan_plus_utility_id + date_submitted, data=., sum) -> leads.by.plan_plus_utility_id     
                
                lbp.spread <- spread(leads.by.plan_plus_utility_id,key = original_plan_plus_utility_id, value = n, fill = 0)
                
                lbp.zoo <- zoo(lbp.spread[,-1], lbp.spread[,1])
                
                merge(lbp.zoo, zoo(1, seq(today() - daysback, today(), by = 1)), all = TRUE) %>% 
                    .[,-(ncol(.))] %>%
                    fortify.zoo(.) %>%
                    reshape2::melt("Index") %>%
                    dplyr::rename(date_submitted = Index, original_plan_plus_utility_id = variable, n = value) %>%
                    separate(original_plan_plus_utility_id, into = c("original_plan_id", "utility_id"), sep = "-") -> lbp 
                
                
                
                til.i <- top.impacts.leads[k,]
                ticr.i <- top.impacts.cr[k,]
                
                if(between(til.i$Average_Relative_Effect, ticr.i$Avg_Rel_Eff_Lo_90_Conf_Interval, ticr.i$Avg_Rel_Eff_Up_90_Conf_Interval)){
                    
                    
                    leads.ct[leads.ct$utility_name_id == top.impacts.leads[k, "utility_name_id"],] %>%
                        filter(product == top.impacts.leads[k, "product"]) %>%
                        .[,!names(.) %in% c("utility_name_id", "product")] -> leads.i
                    
                    lbp %>%
                        filter(utility_id == top.impacts.leads[k, "utility_id"]) %>%
                        .[,!names(.) %in% c("utility_id")] %>%
                        spread(key = original_plan_id, value = n, fill = 0) -> lbp.i
                    
                    for.cor.i <- merge(leads.i, lbp.i, by = "date_submitted")
                    
                    arrange(reshape2::melt(cor(for.cor.i$n, for.cor.i[,-1:-2])),desc(value)) %>%
                        .[,!names(.) %in% c("Var1")] %>%
                        dplyr::rename(original_plan_id = Var2, cor = value) -> cor.i
                    
                    lbp.i %>%
                        .[,names(.) %in% c("date_submitted", cor.i$original_plan_id[1:5])] %>%
                        reshape2::melt("date_submitted") %>%
                        dplyr::rename(original_plan_id = variable, n = value) -> top.lbp
                    
                    changes %>%
                        filter(utility_name_id == top.impacts.leads[k, "utility_name_id"]) %>%
                        filter(original_plan_id == as.numeric(cor.i$original_plan_id[1])) -> changes.i
                    
                    changes.i <- arrange(changes.i[!duplicated(changes.i),], plan_audit_id)
                    
                    if(dim(changes.i)[1] == 0){
                        
                        paste0(" ")
                        
                    } else {
                        if(between(changes.i[1,"date"], change.date.i - 3, change.date.i + 3)){
                            
                            new_plan_status = changes.i[changes.i$plan_audit_id == max(changes.i$plan_audit_id), "plan_status_name"]
                            
                            new_plan_status
                            
                        } else {
                            paste0(" ")
                        }
                        
                    }
                    
                } else{
                    paste0(" ")
                }
                
            })
            
            output$plan_id7.tab <- renderText({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                leads <- get.causal.impact.data()[[7]]
                
                
                k <- 7
                
                change.date.i <- top.impacts.leads[k, "graph.date"]
                
                leads %>%
                    mutate(n = 1) %>%
                    unite(original_plan_plus_utility_id, original_plan_id, utility_id, sep = "-") %>%
                    aggregate(n~original_plan_plus_utility_id + date_submitted, data=., sum) -> leads.by.plan_plus_utility_id     
                
                lbp.spread <- spread(leads.by.plan_plus_utility_id,key = original_plan_plus_utility_id, value = n, fill = 0)
                
                lbp.zoo <- zoo(lbp.spread[,-1], lbp.spread[,1])
                
                merge(lbp.zoo, zoo(1, seq(today() - daysback, today(), by = 1)), all = TRUE) %>% 
                    .[,-(ncol(.))] %>%
                    fortify.zoo(.) %>%
                    reshape2::melt("Index") %>%
                    dplyr::rename(date_submitted = Index, original_plan_plus_utility_id = variable, n = value) %>%
                    separate(original_plan_plus_utility_id, into = c("original_plan_id", "utility_id"), sep = "-") -> lbp 
                
                
                
                til.i <- top.impacts.leads[k,]
                ticr.i <- top.impacts.cr[k,]
                
                if(between(til.i$Average_Relative_Effect, ticr.i$Avg_Rel_Eff_Lo_90_Conf_Interval, ticr.i$Avg_Rel_Eff_Up_90_Conf_Interval)){
                    
                    
                    leads.ct[leads.ct$utility_name_id == top.impacts.leads[k, "utility_name_id"],] %>%
                        filter(product == top.impacts.leads[k, "product"]) %>%
                        .[,!names(.) %in% c("utility_name_id", "product")] -> leads.i
                    
                    lbp %>%
                        filter(utility_id == top.impacts.leads[k, "utility_id"]) %>%
                        .[,!names(.) %in% c("utility_id")] %>%
                        spread(key = original_plan_id, value = n, fill = 0) -> lbp.i
                    
                    for.cor.i <- merge(leads.i, lbp.i, by = "date_submitted")
                    
                    arrange(reshape2::melt(cor(for.cor.i$n, for.cor.i[,-1:-2])),desc(value)) %>%
                        .[,!names(.) %in% c("Var1")] %>%
                        dplyr::rename(original_plan_id = Var2, cor = value) -> cor.i
                    
                    lbp.i %>%
                        .[,names(.) %in% c("date_submitted", cor.i$original_plan_id[1:5])] %>%
                        reshape2::melt("date_submitted") %>%
                        dplyr::rename(original_plan_id = variable, n = value) -> top.lbp
                    
                    changes %>%
                        filter(utility_name_id == top.impacts.leads[k, "utility_name_id"]) %>%
                        filter(original_plan_id == as.numeric(cor.i$original_plan_id[1])) -> changes.i
                    
                    changes.i <- arrange(changes.i[!duplicated(changes.i),], plan_audit_id)
                    
                    
                    if(dim(changes.i)[1] == 0){
                        
                        paste0(" ")
                        
                    } else {
                        if(between(changes.i[1,"date"], change.date.i - 3, change.date.i + 3)){
                            
                            as.character(paste("Plan ID: ", changes.i[1, c("original_plan_id")], sep = ""))
                            
                        } else {
                            paste0(" ")
                        }
                        
                    }
                    
                } else{
                    paste0(" ")
                }
                
                
            })
            
            
            
            output$price_change7 <- renderText({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                leads <- get.causal.impact.data()[[7]]
                
                
                k <- 7
                
                change.date.i <- top.impacts.leads[k, "graph.date"]
                
                leads %>%
                    mutate(n = 1) %>%
                    unite(original_plan_plus_utility_id, original_plan_id, utility_id, sep = "-") %>%
                    aggregate(n~original_plan_plus_utility_id + date_submitted, data=., sum) -> leads.by.plan_plus_utility_id     
                
                lbp.spread <- spread(leads.by.plan_plus_utility_id,key = original_plan_plus_utility_id, value = n, fill = 0)
                
                lbp.zoo <- zoo(lbp.spread[,-1], lbp.spread[,1])
                
                merge(lbp.zoo, zoo(1, seq(today() - daysback, today(), by = 1)), all = TRUE) %>% 
                    .[,-(ncol(.))] %>%
                    fortify.zoo(.) %>%
                    reshape2::melt("Index") %>%
                    dplyr::rename(date_submitted = Index, original_plan_plus_utility_id = variable, n = value) %>%
                    separate(original_plan_plus_utility_id, into = c("original_plan_id", "utility_id"), sep = "-") -> lbp 
                
                
                
                til.i <- top.impacts.leads[k,]
                ticr.i <- top.impacts.cr[k,]
                
                if(between(til.i$Average_Relative_Effect, ticr.i$Avg_Rel_Eff_Lo_90_Conf_Interval, ticr.i$Avg_Rel_Eff_Up_90_Conf_Interval)){
                    
                    
                    leads.ct[leads.ct$utility_name_id == top.impacts.leads[k, "utility_name_id"],] %>%
                        filter(product == top.impacts.leads[k, "product"]) %>%
                        .[,!names(.) %in% c("utility_name_id", "product")] -> leads.i
                    
                    lbp %>%
                        filter(utility_id == top.impacts.leads[k, "utility_id"]) %>%
                        .[,!names(.) %in% c("utility_id")] %>%
                        spread(key = original_plan_id, value = n, fill = 0) -> lbp.i
                    
                    for.cor.i <- merge(leads.i, lbp.i, by = "date_submitted")
                    
                    arrange(reshape2::melt(cor(for.cor.i$n, for.cor.i[,-1:-2])),desc(value)) %>%
                        .[,!names(.) %in% c("Var1")] %>%
                        dplyr::rename(original_plan_id = Var2, cor = value) -> cor.i
                    
                    lbp.i %>%
                        .[,names(.) %in% c("date_submitted", cor.i$original_plan_id[1:5])] %>%
                        reshape2::melt("date_submitted") %>%
                        dplyr::rename(original_plan_id = variable, n = value) -> top.lbp
                    
                    changes %>%
                        filter(utility_name_id == top.impacts.leads[k, "utility_name_id"]) %>%
                        filter(original_plan_id == as.numeric(cor.i$original_plan_id[1])) -> changes.i
                    
                    changes.i <- arrange(changes.i[!duplicated(changes.i),], plan_audit_id)
                    
                    
                    
                    if(dim(changes.i)[1] == 0){
                        
                        paste0(" ")
                        
                    } else {
                        if(between(changes.i[1,"date"], change.date.i - 3, change.date.i + 3)){
                            
                            old_price = changes.i[changes.i$plan_audit_id == min(changes.i$plan_audit_id), "plan_price"]
                            new_price = changes.i[changes.i$plan_audit_id == max(changes.i$plan_audit_id), "plan_price"]
                            
                            paste(old_price, "¢", " -> ", new_price, "¢", sep = "")
                            
                        } else {
                            paste0(" ")
                        }
                        
                    }
                    
                } else{
                    paste0(" ")
                }
                
                
            })
            
            
            
            
            
            
            output$change.7 <- renderInfoBox({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                leads <- get.causal.impact.data()[[7]]
                
                k <- 7
                
                change.date.i <- max(changes[changes$change_index == top.impacts.leads$change_index[k], "date"])
                
                product.i <- unique(changes[changes$change_index == top.impacts.leads$change_index[k], "product"])
                utility.i <- paste(unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility"]), unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility_id"]), sep = " - ")
                
                leads.ct %>%
                    filter(utility_name_id == utility.i) %>%
                    filter(product == product.i) -> leads.i
                
                leads.ts <- zoo(leads.i[,!names(leads.i) %in% c("date_submitted", "utility_name_id", "product")], order.by = leads.i$date_submitted)
                pre.i <- c(min(leads.i$date_submitted), change.date.i)
                post.i <- c(change.date.i + 1, max(leads.i$date_submitted))
                impact.i <- try.Causal.Impact(time_series = leads.ts, pre_date = pre.i, post_date = post.i, conf_int = 0.05)
                
                if(impact.i$summary$AbsEffect[1] < 0){
                    subt <- "Lead Volume Decline"
                    ico <- icon("arrow-down")
                } else {
                    subt <- "Lead Volume Increase"
                    ico <- icon("arrow-up")
                }
                
                infoBox(
                    "What occurred:", subt, icon = ico,
                    color = "purple"
                )
                
            })
            
            output$utility.7 <- renderInfoBox({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                
                k <- 7
                infoBox(
                    "Where:", paste(top.impacts.leads[k, c("state")], top.impacts.leads[k, c("utility")], sep = " - "), icon = icon("plug", "fa-0.5x"),
                    color = "green"
                )        
            })
            
            output$product.7 <- renderInfoBox({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                
                k <- 7
                if(top.impacts.leads[k, c("product")] == "Electricity"){
                    ico <- icon("bolt", "fa-0.5x")
                } else{
                    ico <- icon("fire", "fa-0.5x")
                }
                infoBox(
                    "Which product:", top.impacts.leads[k, c("product")], icon = ico,
                    color = "yellow"
                )        
            })
            
            
            output$causal.impact.cr.plot7 <- renderPlot({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                
                changes.unique <- changes[!duplicated(changes[,c("date", "utility_id")]),]
                unique.index <- unique(changes.unique$change_index)
                
                k <- 7
                
                change.date.i <- max(changes[changes$change_index == top.impacts.leads$change_index[k], "date"])
                
                product.i <- unique(changes[changes$change_index == top.impacts.leads$change_index[k], "product"])
                utility.i <- paste(unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility"]), unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility_id"]), sep = " - ")
                
                cr.ct %>%
                    filter(utility_name_id == utility.i) %>%
                    filter(product == product.i) -> leads.i
                
                leads.ts <- zoo(leads.i[,!names(leads.i) %in% c("date_submitted", "utility_name_id", "product")], order.by = leads.i$date_submitted)
                pre.i <- c(min(leads.i$date_submitted), change.date.i)
                post.i <- c(change.date.i + 1, max(leads.i$date_submitted))
                impact.i <- try.Causal.Impact(time_series = leads.ts, pre_date = pre.i, post_date = post.i, conf_int = 0.05)
                plot(impact.i)
            })
            
            
            output$causal.impact.cr.summary.EXCLUDE <- renderPrint({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                
                changes.unique <- changes[!duplicated(changes[,c("date", "utility_id")]),]
                unique.index <- unique(changes.unique$change_index)
                
                k <- 7
                
                change.date.i <- max(changes[changes$change_index == top.impacts.leads$change_index[k], "date"])
                
                product.i <- unique(changes[changes$change_index == top.impacts.leads$change_index[k], "product"])
                utility.i <- paste(unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility"]), unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility_id"]), sep = " - ")
                
                cr.ct %>%
                    filter(utility_name_id == utility.i) %>%
                    filter(product == product.i) -> leads.i
                
                leads.ts <- zoo(leads.i[,!names(leads.i) %in% c("date_submitted", "utility_name_id", "product")], order.by = leads.i$date_submitted)
                pre.i <- c(min(leads.i$date_submitted), change.date.i)
                post.i <- c(change.date.i + 1, max(leads.i$date_submitted))
                impact.i <- try.Causal.Impact(time_series = leads.ts, pre_date = pre.i, post_date = post.i, conf_int = 0.05)
                summary(impact.i)
            })        
            
            
            output$causal.impact.cr.summary7 <- renderPrint({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                
                changes.unique <- changes[!duplicated(changes[,c("date", "utility_id")]),]
                unique.index <- unique(changes.unique$change_index)
                
                k <- 7
                
                change.date.i <- max(changes[changes$change_index == top.impacts.leads$change_index[k], "date"])
                
                product.i <- unique(changes[changes$change_index == top.impacts.leads$change_index[k], "product"])
                utility.i <- paste(unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility"]), unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility_id"]), sep = " - ")
                cr.ct %>%
                    filter(utility_name_id == utility.i) %>%
                    filter(product == product.i) -> leads.i
                
                leads.ts <- zoo(leads.i[,!names(leads.i) %in% c("date_submitted", "utility_name_id", "product")], order.by = leads.i$date_submitted)
                pre.i <- c(min(leads.i$date_submitted), change.date.i)
                post.i <- c(change.date.i + 1, max(leads.i$date_submitted))
                impact.i <- try.Causal.Impact(time_series = leads.ts, pre_date = pre.i, post_date = post.i, conf_int = 0.05)
                summary(impact.i)
            })
            
            output$causal.impact.leads.summary7 <- renderPrint({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                
                changes.unique <- changes[!duplicated(changes[,c("date", "utility_id")]),]
                unique.index <- unique(changes.unique$change_index)
                
                k <- 7
                
                change.date.i <- max(changes[changes$change_index == top.impacts.leads$change_index[k], "date"])
                
                product.i <- unique(changes[changes$change_index == top.impacts.leads$change_index[k], "product"])
                utility.i <- paste(unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility"]), unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility_id"]), sep = " - ")
                leads.ct %>%
                    filter(utility_name_id == utility.i) %>%
                    filter(product == product.i) -> leads.i
                
                leads.ts <- zoo(leads.i[,!names(leads.i) %in% c("date_submitted", "utility_name_id", "product")], order.by = leads.i$date_submitted)
                pre.i <- c(min(leads.i$date_submitted), change.date.i)
                post.i <- c(change.date.i + 1, max(leads.i$date_submitted))
                impact.i <- try.Causal.Impact(time_series = leads.ts, pre_date = pre.i, post_date = post.i, conf_int = 0.05)
                summary(impact.i)
            })
            
            
            output$causal.impact.leads.plot7 <- renderPlot({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                
                changes.unique <- changes[!duplicated(changes[,c("date", "utility_id")]),]
                unique.index <- unique(changes.unique$change_index)
                
                k <- 7
                
                change.date.i <- max(changes[changes$change_index == top.impacts.leads$change_index[k], "date"])
                
                product.i <- unique(changes[changes$change_index == top.impacts.leads$change_index[k], "product"])
                utility.i <- paste(unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility"]), unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility_id"]), sep = " - ")
                leads.ct %>%
                    filter(utility_name_id == utility.i) %>%
                    filter(product == product.i) -> leads.i
                
                leads.ts <- zoo(leads.i[,!names(leads.i) %in% c("date_submitted", "utility_name_id", "product")], order.by = leads.i$date_submitted)
                pre.i <- c(min(leads.i$date_submitted), change.date.i)
                post.i <- c(change.date.i + 1, max(leads.i$date_submitted))
                impact.i <- try.Causal.Impact(time_series = leads.ts, pre_date = pre.i, post_date = post.i, conf_int = 0.05)
                plot(impact.i)
            })
            
            
            output$complete.plan.changes7 <- renderPlotly({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                
                changes.unique <- changes[!duplicated(changes[,c("date", "utility_id")]),]
                unique.index <- unique(changes.unique$change_index)
                
                k <- 7
                
                change.date.i <- max(changes[changes$change_index == top.impacts.leads$change_index[k], "date"])
                
                product.i <- unique(changes[changes$change_index == top.impacts.leads$change_index[k], "product"])
                utility.i <- paste(unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility"]), unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility_id"]), sep = " - ")
                
                utility.name.i <- unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility_name_id"])
                changes[changes$utility_name_id == utility.name.i, ] %>%
                    filter(graph.date > (today()-(daysback+1))) %>%
                    mutate(Supplier_Plan = paste(change, supplier, plan_name, sep = " - ")) -> changes.full.i
                
                daily.prices %>%
                    filter(utility_name_id == utility.i) %>%
                    filter(product == product.i) -> daily.prices.i
                
                
                
                p <- ggplot() + 
                    geom_line(data = daily.prices.i, aes(date, plan_price.mean)) +
                    geom_line(data = daily.prices.i, aes(date, plan_price.max)) +
                    geom_line(data = daily.prices.i, aes(date, plan_price.min)) +
                    geom_line(data = changes.full.i[changes.full.i$change == "plan_change",], aes(graph.date, plan_price, col = Supplier_Plan)) +
                    geom_point(data = changes.full.i[changes.full.i$change == "plan_end",], aes(graph.date, plan_price, col = Supplier_Plan)) + 
                    geom_point(data = changes.full.i[changes.full.i$change == "new_plan",], aes(graph.date, plan_price, col = Supplier_Plan)) +
                    geom_vline(xintercept = as.numeric(change.date.i), linetype = "longdash") +
                    ggtitle(paste("All Plan Changes Around", change.date.i, sep = " ")) +
                    xlab("Date") +
                    ylab("Plan Price (cents)") +
                    theme(legend.position = "none", panel.background=element_blank(), panel.grid.major = element_line(colour = "grey"))
                
                ggplotly(p)
            })
            
            
            
            
            
            output$plan_breakdown7 <- renderPlot({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                leads <- get.causal.impact.data()[[7]]
                sessions.ct <- get.causal.impact.data()[[8]]     
                plan.inv <- get.causal.impact.data()[[9]]
                
                
                changes.unique <- changes[!duplicated(changes[,c("date", "utility_id")]),]
                unique.index <- unique(changes.unique$change_index)
                
                k <- 7
                
                change.date.i <- max(changes[changes$change_index == top.impacts.leads$change_index[k], "date"])
                
                product.i <- unique(changes[changes$change_index == top.impacts.leads$change_index[k], "product"])
                utility.i <- paste(unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility"]), unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility_id"]), sep = " - ")
                
                utility.name.i <- unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility_name_id"])
                changes[changes$utility_name_id == utility.name.i, ] %>%
                    filter(graph.date > (today()-(daysback+1))) %>%
                    mutate(suplan = paste(change, supplier, plan_name, sep = " - ")) -> changes.full.i
                
                daily.prices %>%
                    filter(utility_name_id == utility.i) %>%
                    filter(product == product.i) -> daily.prices.i
                
                
                leads %>%
                    mutate(n = 1) %>%
                    unite(original_plan_plus_utility_id, original_plan_id, utility_id, sep = "-") %>%
                    aggregate(n~original_plan_plus_utility_id + date_submitted, data=., sum) -> leads.by.plan_plus_utility_id     
                
                lbp.spread <- spread(leads.by.plan_plus_utility_id,key = original_plan_plus_utility_id, value = n, fill = 0)
                
                lbp.zoo <- zoo(lbp.spread[,-1], lbp.spread[,1])
                
                merge(lbp.zoo, zoo(1, seq(today() - daysback, today(), by = 1)), all = TRUE) %>% 
                    .[,-(ncol(.))] %>%
                    fortify.zoo(.) %>%
                    reshape2::melt("Index") %>%
                    dplyr::rename(date_submitted = Index, original_plan_plus_utility_id = variable, n = value) %>%
                    separate(original_plan_plus_utility_id, into = c("original_plan_id", "utility_id"), sep = "-") -> lbp 
                
                
                
                til.i <- top.impacts.leads[k,]
                ticr.i <- top.impacts.cr[k,]
                
                if(between(til.i$Average_Relative_Effect, ticr.i$Avg_Rel_Eff_Lo_90_Conf_Interval, ticr.i$Avg_Rel_Eff_Up_90_Conf_Interval)){
                    
                    
                    leads.ct[leads.ct$utility_name_id == top.impacts.leads[k, "utility_name_id"],] %>%
                        filter(product == top.impacts.leads[k, "product"]) %>%
                        .[,!names(.) %in% c("utility_name_id", "product")] -> leads.i
                    
                    lbp %>%
                        filter(utility_id == top.impacts.leads[k, "utility_id"]) %>%
                        .[,!names(.) %in% c("utility_id")] %>%
                        spread(key = original_plan_id, value = n, fill = 0) -> lbp.i
                    
                    for.cor.i <- merge(leads.i, lbp.i, by = "date_submitted")
                    
                    arrange(reshape2::melt(cor(for.cor.i$n, for.cor.i[,-1:-2])),desc(value)) %>%
                        .[,!names(.) %in% c("Var1")] %>%
                        dplyr::rename(original_plan_id = Var2, cor = value) -> cor.i
                    
                    lbp.i %>%
                        .[,names(.) %in% c("date_submitted", cor.i$original_plan_id[1:5])] %>%
                        reshape2::melt("date_submitted") %>%
                        dplyr::rename(original_plan_id = variable, n = value) %>%
                        merge(unique(plan.inv[,c("supplier", "plan_name", "original_plan_id")]), by = "original_plan_id", all.x = TRUE) %>%
                        unite(suplan, supplier, plan_name, sep = " - ", remove = TRUE) %>%
                        .[,!names(.) %in% c("original_plan_id")] -> top.lbp
                    
                    changes %>%
                        filter(utility_name_id == top.impacts.leads[k, "utility_name_id"]) %>%
                        filter(original_plan_id == as.numeric(cor.i$original_plan_id[1])) -> changes.i
                    
                    changes.i <- arrange(changes.i[!duplicated(changes.i),], plan_audit_id)
                    
                    leads.i %>%
                        mutate(suplan = "Total Leads") %>%
                        .[,c("date_submitted", "n", "suplan")] %>%
                        rbind(top.lbp) -> leads.for.plot
                    
                    leads.for.plot$suplan <- as.character(leads.for.plot$suplan)
                    leads.for.plot$suplan <- factor(leads.for.plot$suplan, levels = c("Total Leads", unique(top.lbp$suplan)))
                    
                    names(leads.for.plot) <- c("date_submitted", "n", "suplan")
                    
                    ggplot(leads.for.plot, aes(date_submitted, n)) + 
                        geom_line() + 
                        facet_wrap(~suplan, ncol = 1) + 
                        ggtitle("Lead Volume of Top Correlating Plans") +
                        theme(legend.position = "none", panel.background=element_blank(), panel.grid.major = element_line(colour = "grey"), strip.text.y = element_text(angle=0), plot.title = element_text(hjust = 0.5, size = 18))+
                        xlab("Date") +
                        ylab("Lead Volume") +
                        geom_vline(xintercept = as.numeric(change.date.i), linetype = "longdash")
                    
                } else{
                    sessions.ct %>%
                        filter(utility_name_id == top.impacts.leads[k, "utility_name_id"]) %>%
                        aggregate(uniquePageviews ~ date_submitted, data=., sum) -> sessions.for.plot
                    
                    ggplot(sessions.for.plot, aes(date_submitted, uniquePageviews)) + 
                        geom_line() + 
                        ggtitle(paste("Sessions in ", top.impacts.leads[k, "utility"], sep = "")) +
                        theme(legend.position = "none", panel.background=element_blank(), panel.grid.major = element_line(colour = "grey")) +
                        xlab("Date") +
                        ylab("Sessions") +
                        geom_vline(xintercept = as.numeric(change.date.i), linetype = "longdash") +
                        theme(plot.title = element_text(hjust = 0.5, size = 18)) +
                        stat_smooth(method=loess)
                    
                }
                
                
            })
            
            
            # # # --- # # # --- # # # --- # # # --- # # # ---
            
            #                      TAB 8                    #  
            
            # # # --- # # # --- # # # --- # # # --- # # # ---
            
            
            
            
            
            
            
            output$likelihood.description.8 <- renderText({
                
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                leads <- get.causal.impact.data()[[7]]
                
                
                k <- 8
                
                change.date.i <- top.impacts.leads[k, "graph.date"]
                
                leads %>%
                    mutate(n = 1) %>%
                    unite(original_plan_plus_utility_id, original_plan_id, utility_id, sep = "-") %>%
                    aggregate(n~original_plan_plus_utility_id + date_submitted, data=., sum) -> leads.by.plan_plus_utility_id     
                
                lbp.spread <- spread(leads.by.plan_plus_utility_id,key = original_plan_plus_utility_id, value = n, fill = 0)
                
                lbp.zoo <- zoo(lbp.spread[,-1], lbp.spread[,1])
                
                merge(lbp.zoo, zoo(1, seq(today() - daysback, today(), by = 1)), all = TRUE) %>% 
                    .[,-(ncol(.))] %>%
                    fortify.zoo(.) %>%
                    reshape2::melt("Index") %>%
                    dplyr::rename(date_submitted = Index, original_plan_plus_utility_id = variable, n = value) %>%
                    separate(original_plan_plus_utility_id, into = c("original_plan_id", "utility_id"), sep = "-") -> lbp 
                
                
                til.i <- top.impacts.leads[k,]
                ticr.i <- top.impacts.cr[k,]
                
                if(between(til.i$Average_Relative_Effect, ticr.i$Avg_Rel_Eff_Lo_90_Conf_Interval, ticr.i$Avg_Rel_Eff_Up_90_Conf_Interval)){
                    
                    
                    leads.ct[leads.ct$utility_name_id == top.impacts.leads[k, "utility_name_id"],] %>%
                        filter(product == top.impacts.leads[k, "product"]) %>%
                        .[,!names(.) %in% c("utility_name_id", "product")] -> leads.i
                    
                    lbp %>%
                        filter(utility_id == top.impacts.leads[k, "utility_id"]) %>%
                        .[,!names(.) %in% c("utility_id")] %>%
                        spread(key = original_plan_id, value = n, fill = 0) -> lbp.i
                    
                    for.cor.i <- merge(leads.i, lbp.i, by = "date_submitted")
                    
                    arrange(reshape2::melt(cor(for.cor.i$n, for.cor.i[,-1:-2])),desc(value)) %>%
                        .[,!names(.) %in% c("Var1")] %>%
                        dplyr::rename(original_plan_id = Var2, cor = value) -> cor.i
                    
                    lbp.i %>%
                        .[,names(.) %in% c("date_submitted", cor.i$original_plan_id[1:5])] %>%
                        reshape2::melt("date_submitted") %>%
                        dplyr::rename(original_plan_id = variable, n = value) -> top.lbp
                    
                    changes %>%
                        filter(utility_name_id == top.impacts.leads[k, "utility_name_id"]) %>%
                        filter(original_plan_id == as.numeric(cor.i$original_plan_id[1])) -> changes.i
                    
                    changes.i <- arrange(changes.i[!duplicated(changes.i),], plan_audit_id)
                    
                    if(dim(changes.i)[1] == 0){
                        
                        description <- paste0("Both lead volume and conversion rate were down, although no particular plan changes occurred that could be traced back to. See Lead Volume of Top Correlating Plans below for possibile causes.")
                        
                    } else {
                        if(between(changes.i[1,"date"], change.date.i - 3, change.date.i + 3)){
                            
                            description <- paste0("This plan change is highly likely to have caused the observed change in lead and conversion rate.")
                            
                        } else {
                            description <- paste0("Both lead volume and conversion rate were down, although no particular plan changes occurred that could be traced back to. See Lead Volume of Top Correlating Plans below for possibile causes.")
                        }
                    }
                    
                } else{
                    description <- paste0("No plan changes were found relating to this impact, and conversion rate either stayed relatively the same or went up while leads went down. Therefore the impact can be traced back to a change in traffic in this utility zone.")
                }
                
                description
                
            })
            
            
            
            
            
            output$plan_name8.tab <- renderText({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                leads <- get.causal.impact.data()[[7]]
                
                
                k <- 8
                
                change.date.i <- top.impacts.leads[k, "graph.date"]
                
                leads %>%
                    mutate(n = 1) %>%
                    unite(original_plan_plus_utility_id, original_plan_id, utility_id, sep = "-") %>%
                    aggregate(n~original_plan_plus_utility_id + date_submitted, data=., sum) -> leads.by.plan_plus_utility_id     
                
                lbp.spread <- spread(leads.by.plan_plus_utility_id,key = original_plan_plus_utility_id, value = n, fill = 0)
                
                lbp.zoo <- zoo(lbp.spread[,-1], lbp.spread[,1])
                
                merge(lbp.zoo, zoo(1, seq(today() - daysback, today(), by = 1)), all = TRUE) %>% 
                    .[,-(ncol(.))] %>%
                    fortify.zoo(.) %>%
                    reshape2::melt("Index") %>%
                    dplyr::rename(date_submitted = Index, original_plan_plus_utility_id = variable, n = value) %>%
                    separate(original_plan_plus_utility_id, into = c("original_plan_id", "utility_id"), sep = "-") -> lbp 
                
                
                
                
                
                til.i <- top.impacts.leads[k,]
                ticr.i <- top.impacts.cr[k,]
                
                if(between(til.i$Average_Relative_Effect, ticr.i$Avg_Rel_Eff_Lo_90_Conf_Interval, ticr.i$Avg_Rel_Eff_Up_90_Conf_Interval)){
                    
                    
                    leads.ct[leads.ct$utility_name_id == top.impacts.leads[k, "utility_name_id"],] %>%
                        filter(product == top.impacts.leads[k, "product"]) %>%
                        .[,!names(.) %in% c("utility_name_id", "product")] -> leads.i
                    
                    lbp %>%
                        filter(utility_id == top.impacts.leads[k, "utility_id"]) %>%
                        .[,!names(.) %in% c("utility_id")] %>%
                        spread(key = original_plan_id, value = n, fill = 0) -> lbp.i
                    
                    for.cor.i <- merge(leads.i, lbp.i, by = "date_submitted")
                    
                    arrange(reshape2::melt(cor(for.cor.i$n, for.cor.i[,-1:-2])),desc(value)) %>%
                        .[,!names(.) %in% c("Var1")] %>%
                        dplyr::rename(original_plan_id = Var2, cor = value) -> cor.i
                    
                    lbp.i %>%
                        .[,names(.) %in% c("date_submitted", cor.i$original_plan_id[1:5])] %>%
                        reshape2::melt("date_submitted") %>%
                        dplyr::rename(original_plan_id = variable, n = value) -> top.lbp
                    
                    changes %>%
                        filter(utility_name_id == top.impacts.leads[k, "utility_name_id"]) %>%
                        filter(original_plan_id == as.numeric(cor.i$original_plan_id[1])) -> changes.i
                    
                    changes.i <- arrange(changes.i[!duplicated(changes.i),], plan_audit_id)
                    
                    if(dim(changes.i)[1] == 0){
                        
                        paste0("Multiple Possibilities")
                        
                    } else {
                        if(between(changes.i[1,"date"], change.date.i - 3, change.date.i + 3)){
                            
                            as.character(changes.i[1, c("plan_name")])
                            
                        } else {
                            paste0("Multiple Possibilities")
                        }
                        
                    }
                    
                } else{
                    paste0("Traffic Change")
                }
                
                
                
            })
            
            
            output$plan_term8.tab <- renderText({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                leads <- get.causal.impact.data()[[7]]
                
                
                k <- 8
                
                change.date.i <- top.impacts.leads[k, "graph.date"]
                
                leads %>%
                    mutate(n = 1) %>%
                    unite(original_plan_plus_utility_id, original_plan_id, utility_id, sep = "-") %>%
                    aggregate(n~original_plan_plus_utility_id + date_submitted, data=., sum) -> leads.by.plan_plus_utility_id     
                
                lbp.spread <- spread(leads.by.plan_plus_utility_id,key = original_plan_plus_utility_id, value = n, fill = 0)
                
                lbp.zoo <- zoo(lbp.spread[,-1], lbp.spread[,1])
                
                merge(lbp.zoo, zoo(1, seq(today() - daysback, today(), by = 1)), all = TRUE) %>% 
                    .[,-(ncol(.))] %>%
                    fortify.zoo(.) %>%
                    reshape2::melt("Index") %>%
                    dplyr::rename(date_submitted = Index, original_plan_plus_utility_id = variable, n = value) %>%
                    separate(original_plan_plus_utility_id, into = c("original_plan_id", "utility_id"), sep = "-") -> lbp 
                
                
                
                til.i <- top.impacts.leads[k,]
                ticr.i <- top.impacts.cr[k,]
                
                if(between(til.i$Average_Relative_Effect, ticr.i$Avg_Rel_Eff_Lo_90_Conf_Interval, ticr.i$Avg_Rel_Eff_Up_90_Conf_Interval)){
                    
                    
                    leads.ct[leads.ct$utility_name_id == top.impacts.leads[k, "utility_name_id"],] %>%
                        filter(product == top.impacts.leads[k, "product"]) %>%
                        .[,!names(.) %in% c("utility_name_id", "product")] -> leads.i
                    
                    lbp %>%
                        filter(utility_id == top.impacts.leads[k, "utility_id"]) %>%
                        .[,!names(.) %in% c("utility_id")] %>%
                        spread(key = original_plan_id, value = n, fill = 0) -> lbp.i
                    
                    for.cor.i <- merge(leads.i, lbp.i, by = "date_submitted")
                    
                    arrange(reshape2::melt(cor(for.cor.i$n, for.cor.i[,-1:-2])),desc(value)) %>%
                        .[,!names(.) %in% c("Var1")] %>%
                        dplyr::rename(original_plan_id = Var2, cor = value) -> cor.i
                    
                    lbp.i %>%
                        .[,names(.) %in% c("date_submitted", cor.i$original_plan_id[1:5])] %>%
                        reshape2::melt("date_submitted") %>%
                        dplyr::rename(original_plan_id = variable, n = value) -> top.lbp
                    
                    changes %>%
                        filter(utility_name_id == top.impacts.leads[k, "utility_name_id"]) %>%
                        filter(original_plan_id == as.numeric(cor.i$original_plan_id[1])) -> changes.i
                    
                    changes.i <- arrange(changes.i[!duplicated(changes.i),], plan_audit_id)
                    
                    if(dim(changes.i)[1] == 0){
                        
                        paste0(" ")
                        
                    } else {
                        if(between(changes.i[1,"date"], change.date.i - 3, change.date.i + 3)){
                            
                            as.character(paste(changes.i[1, c("contract_term")], " Months ", changes.i[1, c("contract_type")], sep = ""))
                            
                        } else {
                            paste0(" ")
                        }
                        
                    }
                    
                } else{
                    paste0(" ")
                }
                
            }) 
            
            
            output$plan_supplier8.tab <- renderText({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                leads <- get.causal.impact.data()[[7]]
                
                
                k <- 8
                
                change.date.i <- top.impacts.leads[k, "graph.date"]
                
                leads %>%
                    mutate(n = 1) %>%
                    unite(original_plan_plus_utility_id, original_plan_id, utility_id, sep = "-") %>%
                    aggregate(n~original_plan_plus_utility_id + date_submitted, data=., sum) -> leads.by.plan_plus_utility_id     
                
                lbp.spread <- spread(leads.by.plan_plus_utility_id,key = original_plan_plus_utility_id, value = n, fill = 0)
                
                lbp.zoo <- zoo(lbp.spread[,-1], lbp.spread[,1])
                
                merge(lbp.zoo, zoo(1, seq(today() - daysback, today(), by = 1)), all = TRUE) %>% 
                    .[,-(ncol(.))] %>%
                    fortify.zoo(.) %>%
                    reshape2::melt("Index") %>%
                    dplyr::rename(date_submitted = Index, original_plan_plus_utility_id = variable, n = value) %>%
                    separate(original_plan_plus_utility_id, into = c("original_plan_id", "utility_id"), sep = "-") -> lbp 
                
                
                
                til.i <- top.impacts.leads[k,]
                ticr.i <- top.impacts.cr[k,]
                
                if(between(til.i$Average_Relative_Effect, ticr.i$Avg_Rel_Eff_Lo_90_Conf_Interval, ticr.i$Avg_Rel_Eff_Up_90_Conf_Interval)){
                    
                    
                    leads.ct[leads.ct$utility_name_id == top.impacts.leads[k, "utility_name_id"],] %>%
                        filter(product == top.impacts.leads[k, "product"]) %>%
                        .[,!names(.) %in% c("utility_name_id", "product")] -> leads.i
                    
                    lbp %>%
                        filter(utility_id == top.impacts.leads[k, "utility_id"]) %>%
                        .[,!names(.) %in% c("utility_id")] %>%
                        spread(key = original_plan_id, value = n, fill = 0) -> lbp.i
                    
                    for.cor.i <- merge(leads.i, lbp.i, by = "date_submitted")
                    
                    arrange(reshape2::melt(cor(for.cor.i$n, for.cor.i[,-1:-2])),desc(value)) %>%
                        .[,!names(.) %in% c("Var1")] %>%
                        dplyr::rename(original_plan_id = Var2, cor = value) -> cor.i
                    
                    lbp.i %>%
                        .[,names(.) %in% c("date_submitted", cor.i$original_plan_id[1:5])] %>%
                        reshape2::melt("date_submitted") %>%
                        dplyr::rename(original_plan_id = variable, n = value) -> top.lbp
                    
                    changes %>%
                        filter(utility_name_id == top.impacts.leads[k, "utility_name_id"]) %>%
                        filter(original_plan_id == as.numeric(cor.i$original_plan_id[1])) -> changes.i
                    
                    changes.i <- arrange(changes.i[!duplicated(changes.i),], plan_audit_id)
                    
                    if(dim(changes.i)[1] == 0){
                        
                        paste0(" ")
                        
                    } else {
                        if(between(changes.i[1,"date"], change.date.i - 3, change.date.i + 3)){
                            
                            as.character(changes.i[1, c("supplier")])
                            
                        } else {
                            paste0(" ")
                        }
                        
                    }
                    
                } else{
                    paste0(" ")
                }
                
            })
            
            
            output$plan_active8.tab <- renderText({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                leads <- get.causal.impact.data()[[7]]
                
                
                k <- 8
                
                change.date.i <- top.impacts.leads[k, "graph.date"]
                
                leads %>%
                    mutate(n = 1) %>%
                    unite(original_plan_plus_utility_id, original_plan_id, utility_id, sep = "-") %>%
                    aggregate(n~original_plan_plus_utility_id + date_submitted, data=., sum) -> leads.by.plan_plus_utility_id     
                
                lbp.spread <- spread(leads.by.plan_plus_utility_id,key = original_plan_plus_utility_id, value = n, fill = 0)
                
                lbp.zoo <- zoo(lbp.spread[,-1], lbp.spread[,1])
                
                merge(lbp.zoo, zoo(1, seq(today() - daysback, today(), by = 1)), all = TRUE) %>% 
                    .[,-(ncol(.))] %>%
                    fortify.zoo(.) %>%
                    reshape2::melt("Index") %>%
                    dplyr::rename(date_submitted = Index, original_plan_plus_utility_id = variable, n = value) %>%
                    separate(original_plan_plus_utility_id, into = c("original_plan_id", "utility_id"), sep = "-") -> lbp 
                
                
                
                til.i <- top.impacts.leads[k,]
                ticr.i <- top.impacts.cr[k,]
                
                if(between(til.i$Average_Relative_Effect, ticr.i$Avg_Rel_Eff_Lo_90_Conf_Interval, ticr.i$Avg_Rel_Eff_Up_90_Conf_Interval)){
                    
                    
                    leads.ct[leads.ct$utility_name_id == top.impacts.leads[k, "utility_name_id"],] %>%
                        filter(product == top.impacts.leads[k, "product"]) %>%
                        .[,!names(.) %in% c("utility_name_id", "product")] -> leads.i
                    
                    lbp %>%
                        filter(utility_id == top.impacts.leads[k, "utility_id"]) %>%
                        .[,!names(.) %in% c("utility_id")] %>%
                        spread(key = original_plan_id, value = n, fill = 0) -> lbp.i
                    
                    for.cor.i <- merge(leads.i, lbp.i, by = "date_submitted")
                    
                    arrange(reshape2::melt(cor(for.cor.i$n, for.cor.i[,-1:-2])),desc(value)) %>%
                        .[,!names(.) %in% c("Var1")] %>%
                        dplyr::rename(original_plan_id = Var2, cor = value) -> cor.i
                    
                    lbp.i %>%
                        .[,names(.) %in% c("date_submitted", cor.i$original_plan_id[1:5])] %>%
                        reshape2::melt("date_submitted") %>%
                        dplyr::rename(original_plan_id = variable, n = value) -> top.lbp
                    
                    changes %>%
                        filter(utility_name_id == top.impacts.leads[k, "utility_name_id"]) %>%
                        filter(original_plan_id == as.numeric(cor.i$original_plan_id[1])) -> changes.i
                    
                    changes.i <- arrange(changes.i[!duplicated(changes.i),], plan_audit_id)
                    
                    if(dim(changes.i)[1] == 0){
                        
                        paste0(" ")
                        
                    } else {
                        if(between(changes.i[1,"date"], change.date.i - 3, change.date.i + 3)){
                            
                            new_plan_status = changes.i[changes.i$plan_audit_id == max(changes.i$plan_audit_id), "plan_status_name"]
                            
                            new_plan_status
                            
                        } else {
                            paste0(" ")
                        }
                        
                    }
                    
                } else{
                    paste0(" ")
                }
                
            })
            
            output$plan_id8.tab <- renderText({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                leads <- get.causal.impact.data()[[7]]
                
                
                k <- 8
                
                change.date.i <- top.impacts.leads[k, "graph.date"]
                
                leads %>%
                    mutate(n = 1) %>%
                    unite(original_plan_plus_utility_id, original_plan_id, utility_id, sep = "-") %>%
                    aggregate(n~original_plan_plus_utility_id + date_submitted, data=., sum) -> leads.by.plan_plus_utility_id     
                
                lbp.spread <- spread(leads.by.plan_plus_utility_id,key = original_plan_plus_utility_id, value = n, fill = 0)
                
                lbp.zoo <- zoo(lbp.spread[,-1], lbp.spread[,1])
                
                merge(lbp.zoo, zoo(1, seq(today() - daysback, today(), by = 1)), all = TRUE) %>% 
                    .[,-(ncol(.))] %>%
                    fortify.zoo(.) %>%
                    reshape2::melt("Index") %>%
                    dplyr::rename(date_submitted = Index, original_plan_plus_utility_id = variable, n = value) %>%
                    separate(original_plan_plus_utility_id, into = c("original_plan_id", "utility_id"), sep = "-") -> lbp 
                
                
                
                til.i <- top.impacts.leads[k,]
                ticr.i <- top.impacts.cr[k,]
                
                if(between(til.i$Average_Relative_Effect, ticr.i$Avg_Rel_Eff_Lo_90_Conf_Interval, ticr.i$Avg_Rel_Eff_Up_90_Conf_Interval)){
                    
                    
                    leads.ct[leads.ct$utility_name_id == top.impacts.leads[k, "utility_name_id"],] %>%
                        filter(product == top.impacts.leads[k, "product"]) %>%
                        .[,!names(.) %in% c("utility_name_id", "product")] -> leads.i
                    
                    lbp %>%
                        filter(utility_id == top.impacts.leads[k, "utility_id"]) %>%
                        .[,!names(.) %in% c("utility_id")] %>%
                        spread(key = original_plan_id, value = n, fill = 0) -> lbp.i
                    
                    for.cor.i <- merge(leads.i, lbp.i, by = "date_submitted")
                    
                    arrange(reshape2::melt(cor(for.cor.i$n, for.cor.i[,-1:-2])),desc(value)) %>%
                        .[,!names(.) %in% c("Var1")] %>%
                        dplyr::rename(original_plan_id = Var2, cor = value) -> cor.i
                    
                    lbp.i %>%
                        .[,names(.) %in% c("date_submitted", cor.i$original_plan_id[1:5])] %>%
                        reshape2::melt("date_submitted") %>%
                        dplyr::rename(original_plan_id = variable, n = value) -> top.lbp
                    
                    changes %>%
                        filter(utility_name_id == top.impacts.leads[k, "utility_name_id"]) %>%
                        filter(original_plan_id == as.numeric(cor.i$original_plan_id[1])) -> changes.i
                    
                    changes.i <- arrange(changes.i[!duplicated(changes.i),], plan_audit_id)
                    
                    
                    if(dim(changes.i)[1] == 0){
                        
                        paste0(" ")
                        
                    } else {
                        if(between(changes.i[1,"date"], change.date.i - 3, change.date.i + 3)){
                            
                            as.character(paste("Plan ID: ", changes.i[1, c("original_plan_id")], sep = ""))
                            
                        } else {
                            paste0(" ")
                        }
                        
                    }
                    
                } else{
                    paste0(" ")
                }
                
                
            })
            
            
            
            output$price_change8 <- renderText({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                leads <- get.causal.impact.data()[[7]]
                
                
                k <- 8
                
                change.date.i <- top.impacts.leads[k, "graph.date"]
                
                leads %>%
                    mutate(n = 1) %>%
                    unite(original_plan_plus_utility_id, original_plan_id, utility_id, sep = "-") %>%
                    aggregate(n~original_plan_plus_utility_id + date_submitted, data=., sum) -> leads.by.plan_plus_utility_id     
                
                lbp.spread <- spread(leads.by.plan_plus_utility_id,key = original_plan_plus_utility_id, value = n, fill = 0)
                
                lbp.zoo <- zoo(lbp.spread[,-1], lbp.spread[,1])
                
                merge(lbp.zoo, zoo(1, seq(today() - daysback, today(), by = 1)), all = TRUE) %>% 
                    .[,-(ncol(.))] %>%
                    fortify.zoo(.) %>%
                    reshape2::melt("Index") %>%
                    dplyr::rename(date_submitted = Index, original_plan_plus_utility_id = variable, n = value) %>%
                    separate(original_plan_plus_utility_id, into = c("original_plan_id", "utility_id"), sep = "-") -> lbp 
                
                
                
                til.i <- top.impacts.leads[k,]
                ticr.i <- top.impacts.cr[k,]
                
                if(between(til.i$Average_Relative_Effect, ticr.i$Avg_Rel_Eff_Lo_90_Conf_Interval, ticr.i$Avg_Rel_Eff_Up_90_Conf_Interval)){
                    
                    
                    leads.ct[leads.ct$utility_name_id == top.impacts.leads[k, "utility_name_id"],] %>%
                        filter(product == top.impacts.leads[k, "product"]) %>%
                        .[,!names(.) %in% c("utility_name_id", "product")] -> leads.i
                    
                    lbp %>%
                        filter(utility_id == top.impacts.leads[k, "utility_id"]) %>%
                        .[,!names(.) %in% c("utility_id")] %>%
                        spread(key = original_plan_id, value = n, fill = 0) -> lbp.i
                    
                    for.cor.i <- merge(leads.i, lbp.i, by = "date_submitted")
                    
                    arrange(reshape2::melt(cor(for.cor.i$n, for.cor.i[,-1:-2])),desc(value)) %>%
                        .[,!names(.) %in% c("Var1")] %>%
                        dplyr::rename(original_plan_id = Var2, cor = value) -> cor.i
                    
                    lbp.i %>%
                        .[,names(.) %in% c("date_submitted", cor.i$original_plan_id[1:5])] %>%
                        reshape2::melt("date_submitted") %>%
                        dplyr::rename(original_plan_id = variable, n = value) -> top.lbp
                    
                    changes %>%
                        filter(utility_name_id == top.impacts.leads[k, "utility_name_id"]) %>%
                        filter(original_plan_id == as.numeric(cor.i$original_plan_id[1])) -> changes.i
                    
                    changes.i <- arrange(changes.i[!duplicated(changes.i),], plan_audit_id)
                    
                    
                    
                    if(dim(changes.i)[1] == 0){
                        
                        paste0(" ")
                        
                    } else {
                        if(between(changes.i[1,"date"], change.date.i - 3, change.date.i + 3)){
                            
                            old_price = changes.i[changes.i$plan_audit_id == min(changes.i$plan_audit_id), "plan_price"]
                            new_price = changes.i[changes.i$plan_audit_id == max(changes.i$plan_audit_id), "plan_price"]
                            
                            paste(old_price, "¢", " -> ", new_price, "¢", sep = "")
                            
                        } else {
                            paste0(" ")
                        }
                        
                    }
                    
                } else{
                    paste0(" ")
                }
                
                
            })
            
            
            
            
            
            
            output$change.8 <- renderInfoBox({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                leads <- get.causal.impact.data()[[7]]
                
                k <- 8
                
                change.date.i <- max(changes[changes$change_index == top.impacts.leads$change_index[k], "date"])
                
                product.i <- unique(changes[changes$change_index == top.impacts.leads$change_index[k], "product"])
                utility.i <- paste(unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility"]), unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility_id"]), sep = " - ")
                
                leads.ct %>%
                    filter(utility_name_id == utility.i) %>%
                    filter(product == product.i) -> leads.i
                
                leads.ts <- zoo(leads.i[,!names(leads.i) %in% c("date_submitted", "utility_name_id", "product")], order.by = leads.i$date_submitted)
                pre.i <- c(min(leads.i$date_submitted), change.date.i)
                post.i <- c(change.date.i + 1, max(leads.i$date_submitted))
                impact.i <- try.Causal.Impact(time_series = leads.ts, pre_date = pre.i, post_date = post.i, conf_int = 0.05)
                
                if(impact.i$summary$AbsEffect[1] < 0){
                    subt <- "Lead Volume Decline"
                    ico <- icon("arrow-down")
                } else {
                    subt <- "Lead Volume Increase"
                    ico <- icon("arrow-up")
                }
                
                infoBox(
                    "What occurred:", subt, icon = ico,
                    color = "purple"
                )
                
            })
            
            output$utility.8 <- renderInfoBox({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                
                k <- 8
                infoBox(
                    "Where:", paste(top.impacts.leads[k, c("state")], top.impacts.leads[k, c("utility")], sep = " - "), icon = icon("plug", "fa-0.5x"),
                    color = "green"
                )        
            })
            
            output$product.8 <- renderInfoBox({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                
                k <- 8
                if(top.impacts.leads[k, c("product")] == "Electricity"){
                    ico <- icon("bolt", "fa-0.5x")
                } else{
                    ico <- icon("fire", "fa-0.5x")
                }
                infoBox(
                    "Which product:", top.impacts.leads[k, c("product")], icon = ico,
                    color = "yellow"
                )        
            })
            
            
            output$causal.impact.cr.plot8 <- renderPlot({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                
                changes.unique <- changes[!duplicated(changes[,c("date", "utility_id")]),]
                unique.index <- unique(changes.unique$change_index)
                
                k <- 8
                
                change.date.i <- max(changes[changes$change_index == top.impacts.leads$change_index[k], "date"])
                
                product.i <- unique(changes[changes$change_index == top.impacts.leads$change_index[k], "product"])
                utility.i <- paste(unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility"]), unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility_id"]), sep = " - ")
                
                cr.ct %>%
                    filter(utility_name_id == utility.i) %>%
                    filter(product == product.i) -> leads.i
                
                leads.ts <- zoo(leads.i[,!names(leads.i) %in% c("date_submitted", "utility_name_id", "product")], order.by = leads.i$date_submitted)
                pre.i <- c(min(leads.i$date_submitted), change.date.i)
                post.i <- c(change.date.i + 1, max(leads.i$date_submitted))
                impact.i <- try.Causal.Impact(time_series = leads.ts, pre_date = pre.i, post_date = post.i, conf_int = 0.05)
                plot(impact.i)
            })
            
            
            output$causal.impact.cr.summary.EXCLUDE <- renderPrint({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                
                changes.unique <- changes[!duplicated(changes[,c("date", "utility_id")]),]
                unique.index <- unique(changes.unique$change_index)
                
                k <- 8
                
                change.date.i <- max(changes[changes$change_index == top.impacts.leads$change_index[k], "date"])
                
                product.i <- unique(changes[changes$change_index == top.impacts.leads$change_index[k], "product"])
                utility.i <- paste(unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility"]), unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility_id"]), sep = " - ")
                
                cr.ct %>%
                    filter(utility_name_id == utility.i) %>%
                    filter(product == product.i) -> leads.i
                
                leads.ts <- zoo(leads.i[,!names(leads.i) %in% c("date_submitted", "utility_name_id", "product")], order.by = leads.i$date_submitted)
                pre.i <- c(min(leads.i$date_submitted), change.date.i)
                post.i <- c(change.date.i + 1, max(leads.i$date_submitted))
                impact.i <- try.Causal.Impact(time_series = leads.ts, pre_date = pre.i, post_date = post.i, conf_int = 0.05)
                summary(impact.i)
            })        
            
            
            output$causal.impact.cr.summary8 <- renderPrint({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                
                changes.unique <- changes[!duplicated(changes[,c("date", "utility_id")]),]
                unique.index <- unique(changes.unique$change_index)
                
                k <- 8
                
                change.date.i <- max(changes[changes$change_index == top.impacts.leads$change_index[k], "date"])
                
                product.i <- unique(changes[changes$change_index == top.impacts.leads$change_index[k], "product"])
                utility.i <- paste(unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility"]), unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility_id"]), sep = " - ")
                cr.ct %>%
                    filter(utility_name_id == utility.i) %>%
                    filter(product == product.i) -> leads.i
                
                leads.ts <- zoo(leads.i[,!names(leads.i) %in% c("date_submitted", "utility_name_id", "product")], order.by = leads.i$date_submitted)
                pre.i <- c(min(leads.i$date_submitted), change.date.i)
                post.i <- c(change.date.i + 1, max(leads.i$date_submitted))
                impact.i <- try.Causal.Impact(time_series = leads.ts, pre_date = pre.i, post_date = post.i, conf_int = 0.05)
                summary(impact.i)
            })
            
            output$causal.impact.leads.summary8 <- renderPrint({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                
                changes.unique <- changes[!duplicated(changes[,c("date", "utility_id")]),]
                unique.index <- unique(changes.unique$change_index)
                
                k <- 8
                
                change.date.i <- max(changes[changes$change_index == top.impacts.leads$change_index[k], "date"])
                
                product.i <- unique(changes[changes$change_index == top.impacts.leads$change_index[k], "product"])
                utility.i <- paste(unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility"]), unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility_id"]), sep = " - ")
                leads.ct %>%
                    filter(utility_name_id == utility.i) %>%
                    filter(product == product.i) -> leads.i
                
                leads.ts <- zoo(leads.i[,!names(leads.i) %in% c("date_submitted", "utility_name_id", "product")], order.by = leads.i$date_submitted)
                pre.i <- c(min(leads.i$date_submitted), change.date.i)
                post.i <- c(change.date.i + 1, max(leads.i$date_submitted))
                impact.i <- try.Causal.Impact(time_series = leads.ts, pre_date = pre.i, post_date = post.i, conf_int = 0.05)
                summary(impact.i)
            })
            
            
            output$causal.impact.leads.plot8 <- renderPlot({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                
                changes.unique <- changes[!duplicated(changes[,c("date", "utility_id")]),]
                unique.index <- unique(changes.unique$change_index)
                
                k <- 8
                
                change.date.i <- max(changes[changes$change_index == top.impacts.leads$change_index[k], "date"])
                
                product.i <- unique(changes[changes$change_index == top.impacts.leads$change_index[k], "product"])
                utility.i <- paste(unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility"]), unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility_id"]), sep = " - ")
                leads.ct %>%
                    filter(utility_name_id == utility.i) %>%
                    filter(product == product.i) -> leads.i
                
                leads.ts <- zoo(leads.i[,!names(leads.i) %in% c("date_submitted", "utility_name_id", "product")], order.by = leads.i$date_submitted)
                pre.i <- c(min(leads.i$date_submitted), change.date.i)
                post.i <- c(change.date.i + 1, max(leads.i$date_submitted))
                impact.i <- try.Causal.Impact(time_series = leads.ts, pre_date = pre.i, post_date = post.i, conf_int = 0.05)
                plot(impact.i)
            })
            
            
            output$complete.plan.changes8 <- renderPlotly({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                
                changes.unique <- changes[!duplicated(changes[,c("date", "utility_id")]),]
                unique.index <- unique(changes.unique$change_index)
                
                k <- 8
                
                change.date.i <- max(changes[changes$change_index == top.impacts.leads$change_index[k], "date"])
                
                product.i <- unique(changes[changes$change_index == top.impacts.leads$change_index[k], "product"])
                utility.i <- paste(unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility"]), unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility_id"]), sep = " - ")
                
                utility.name.i <- unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility_name_id"])
                changes[changes$utility_name_id == utility.name.i, ] %>%
                    filter(graph.date > (today()-(daysback+1))) %>%
                    mutate(Supplier_Plan = paste(change, supplier, plan_name, sep = " - ")) -> changes.full.i
                
                daily.prices %>%
                    filter(utility_name_id == utility.i) %>%
                    filter(product == product.i) -> daily.prices.i
                
                
                
                p <- ggplot() + 
                    geom_line(data = daily.prices.i, aes(date, plan_price.mean)) +
                    geom_line(data = daily.prices.i, aes(date, plan_price.max)) +
                    geom_line(data = daily.prices.i, aes(date, plan_price.min)) +
                    geom_line(data = changes.full.i[changes.full.i$change == "plan_change",], aes(graph.date, plan_price, col = Supplier_Plan)) +
                    geom_point(data = changes.full.i[changes.full.i$change == "plan_end",], aes(graph.date, plan_price, col = Supplier_Plan)) + 
                    geom_point(data = changes.full.i[changes.full.i$change == "new_plan",], aes(graph.date, plan_price, col = Supplier_Plan)) +
                    geom_vline(xintercept = as.numeric(change.date.i), linetype = "longdash") +
                    ggtitle(paste("All Plan Changes Around", change.date.i, sep = " ")) +
                    xlab("Date") +
                    ylab("Plan Price (cents)") +
                    theme(legend.position = "none", panel.background=element_blank(), panel.grid.major = element_line(colour = "grey"))
                
                ggplotly(p)
            })
            
            
            
            
            
            output$plan_breakdown8 <- renderPlot({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                leads <- get.causal.impact.data()[[7]]
                sessions.ct <- get.causal.impact.data()[[8]]     
                plan.inv <- get.causal.impact.data()[[9]]
                
                
                changes.unique <- changes[!duplicated(changes[,c("date", "utility_id")]),]
                unique.index <- unique(changes.unique$change_index)
                
                k <- 8
                
                change.date.i <- max(changes[changes$change_index == top.impacts.leads$change_index[k], "date"])
                
                product.i <- unique(changes[changes$change_index == top.impacts.leads$change_index[k], "product"])
                utility.i <- paste(unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility"]), unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility_id"]), sep = " - ")
                
                utility.name.i <- unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility_name_id"])
                changes[changes$utility_name_id == utility.name.i, ] %>%
                    filter(graph.date > (today()-(daysback+1))) %>%
                    mutate(suplan = paste(change, supplier, plan_name, sep = " - ")) -> changes.full.i
                
                daily.prices %>%
                    filter(utility_name_id == utility.i) %>%
                    filter(product == product.i) -> daily.prices.i
                
                
                leads %>%
                    mutate(n = 1) %>%
                    unite(original_plan_plus_utility_id, original_plan_id, utility_id, sep = "-") %>%
                    aggregate(n~original_plan_plus_utility_id + date_submitted, data=., sum) -> leads.by.plan_plus_utility_id     
                
                lbp.spread <- spread(leads.by.plan_plus_utility_id,key = original_plan_plus_utility_id, value = n, fill = 0)
                
                lbp.zoo <- zoo(lbp.spread[,-1], lbp.spread[,1])
                
                merge(lbp.zoo, zoo(1, seq(today() - daysback, today(), by = 1)), all = TRUE) %>% 
                    .[,-(ncol(.))] %>%
                    fortify.zoo(.) %>%
                    reshape2::melt("Index") %>%
                    dplyr::rename(date_submitted = Index, original_plan_plus_utility_id = variable, n = value) %>%
                    separate(original_plan_plus_utility_id, into = c("original_plan_id", "utility_id"), sep = "-") -> lbp 
                
                
                
                til.i <- top.impacts.leads[k,]
                ticr.i <- top.impacts.cr[k,]
                
                if(between(til.i$Average_Relative_Effect, ticr.i$Avg_Rel_Eff_Lo_90_Conf_Interval, ticr.i$Avg_Rel_Eff_Up_90_Conf_Interval)){
                    
                    
                    leads.ct[leads.ct$utility_name_id == top.impacts.leads[k, "utility_name_id"],] %>%
                        filter(product == top.impacts.leads[k, "product"]) %>%
                        .[,!names(.) %in% c("utility_name_id", "product")] -> leads.i
                    
                    lbp %>%
                        filter(utility_id == top.impacts.leads[k, "utility_id"]) %>%
                        .[,!names(.) %in% c("utility_id")] %>%
                        spread(key = original_plan_id, value = n, fill = 0) -> lbp.i
                    
                    for.cor.i <- merge(leads.i, lbp.i, by = "date_submitted")
                    
                    arrange(reshape2::melt(cor(for.cor.i$n, for.cor.i[,-1:-2])),desc(value)) %>%
                        .[,!names(.) %in% c("Var1")] %>%
                        dplyr::rename(original_plan_id = Var2, cor = value) -> cor.i
                    
                    lbp.i %>%
                        .[,names(.) %in% c("date_submitted", cor.i$original_plan_id[1:5])] %>%
                        reshape2::melt("date_submitted") %>%
                        dplyr::rename(original_plan_id = variable, n = value) %>%
                        merge(unique(plan.inv[,c("supplier", "plan_name", "original_plan_id")]), by = "original_plan_id", all.x = TRUE) %>%
                        unite(suplan, supplier, plan_name, sep = " - ", remove = TRUE) %>%
                        .[,!names(.) %in% c("original_plan_id")] -> top.lbp
                    
                    changes %>%
                        filter(utility_name_id == top.impacts.leads[k, "utility_name_id"]) %>%
                        filter(original_plan_id == as.numeric(cor.i$original_plan_id[1])) -> changes.i
                    
                    changes.i <- arrange(changes.i[!duplicated(changes.i),], plan_audit_id)
                    
                    leads.i %>%
                        mutate(suplan = "Total Leads") %>%
                        .[,c("date_submitted", "n", "suplan")] %>%
                        rbind(top.lbp) -> leads.for.plot
                    
                    leads.for.plot$suplan <- as.character(leads.for.plot$suplan)
                    leads.for.plot$suplan <- factor(leads.for.plot$suplan, levels = c("Total Leads", unique(top.lbp$suplan)))
                    
                    names(leads.for.plot) <- c("date_submitted", "n", "suplan")
                    
                    ggplot(leads.for.plot, aes(date_submitted, n)) + 
                        geom_line() + 
                        facet_wrap(~suplan, ncol = 1) + 
                        ggtitle("Lead Volume of Top Correlating Plans") +
                        theme(legend.position = "none", panel.background=element_blank(), panel.grid.major = element_line(colour = "grey"), strip.text.y = element_text(angle=0), plot.title = element_text(hjust = 0.5, size = 18))+
                        xlab("Date") +
                        ylab("Lead Volume") +
                        geom_vline(xintercept = as.numeric(change.date.i), linetype = "longdash")
                    
                } else{
                    sessions.ct %>%
                        filter(utility_name_id == top.impacts.leads[k, "utility_name_id"]) %>%
                        aggregate(uniquePageviews ~ date_submitted, data=., sum) -> sessions.for.plot
                    
                    ggplot(sessions.for.plot, aes(date_submitted, uniquePageviews)) + 
                        geom_line() + 
                        ggtitle(paste("Sessions in ", top.impacts.leads[k, "utility"], sep = "")) +
                        theme(legend.position = "none", panel.background=element_blank(), panel.grid.major = element_line(colour = "grey")) +
                        xlab("Date") +
                        ylab("Sessions") +
                        geom_vline(xintercept = as.numeric(change.date.i), linetype = "longdash") +
                        theme(plot.title = element_text(hjust = 0.5, size = 18)) +
                        stat_smooth(method=loess)
                    
                }
                
                
            })
            
            
            # # # --- # # # --- # # # --- # # # --- # # # ---
            
            #                      TAB 9                    #  
            
            # # # --- # # # --- # # # --- # # # --- # # # ---
            
            
            
            
            
            
            
            output$likelihood.description.9 <- renderText({
                
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                leads <- get.causal.impact.data()[[7]]
                
                
                k <- 9
                
                change.date.i <- top.impacts.leads[k, "graph.date"]
                
                leads %>%
                    mutate(n = 1) %>%
                    unite(original_plan_plus_utility_id, original_plan_id, utility_id, sep = "-") %>%
                    aggregate(n~original_plan_plus_utility_id + date_submitted, data=., sum) -> leads.by.plan_plus_utility_id     
                
                lbp.spread <- spread(leads.by.plan_plus_utility_id,key = original_plan_plus_utility_id, value = n, fill = 0)
                
                lbp.zoo <- zoo(lbp.spread[,-1], lbp.spread[,1])
                
                merge(lbp.zoo, zoo(1, seq(today() - daysback, today(), by = 1)), all = TRUE) %>% 
                    .[,-(ncol(.))] %>%
                    fortify.zoo(.) %>%
                    reshape2::melt("Index") %>%
                    dplyr::rename(date_submitted = Index, original_plan_plus_utility_id = variable, n = value) %>%
                    separate(original_plan_plus_utility_id, into = c("original_plan_id", "utility_id"), sep = "-") -> lbp 
                
                
                til.i <- top.impacts.leads[k,]
                ticr.i <- top.impacts.cr[k,]
                
                if(between(til.i$Average_Relative_Effect, ticr.i$Avg_Rel_Eff_Lo_90_Conf_Interval, ticr.i$Avg_Rel_Eff_Up_90_Conf_Interval)){
                    
                    
                    leads.ct[leads.ct$utility_name_id == top.impacts.leads[k, "utility_name_id"],] %>%
                        filter(product == top.impacts.leads[k, "product"]) %>%
                        .[,!names(.) %in% c("utility_name_id", "product")] -> leads.i
                    
                    lbp %>%
                        filter(utility_id == top.impacts.leads[k, "utility_id"]) %>%
                        .[,!names(.) %in% c("utility_id")] %>%
                        spread(key = original_plan_id, value = n, fill = 0) -> lbp.i
                    
                    for.cor.i <- merge(leads.i, lbp.i, by = "date_submitted")
                    
                    arrange(reshape2::melt(cor(for.cor.i$n, for.cor.i[,-1:-2])),desc(value)) %>%
                        .[,!names(.) %in% c("Var1")] %>%
                        dplyr::rename(original_plan_id = Var2, cor = value) -> cor.i
                    
                    lbp.i %>%
                        .[,names(.) %in% c("date_submitted", cor.i$original_plan_id[1:5])] %>%
                        reshape2::melt("date_submitted") %>%
                        dplyr::rename(original_plan_id = variable, n = value) -> top.lbp
                    
                    changes %>%
                        filter(utility_name_id == top.impacts.leads[k, "utility_name_id"]) %>%
                        filter(original_plan_id == as.numeric(cor.i$original_plan_id[1])) -> changes.i
                    
                    changes.i <- arrange(changes.i[!duplicated(changes.i),], plan_audit_id)
                    
                    if(dim(changes.i)[1] == 0){
                        
                        description <- paste0("Both lead volume and conversion rate were down, although no particular plan changes occurred that could be traced back to. See Lead Volume of Top Correlating Plans below for possibile causes.")
                        
                    } else {
                        if(between(changes.i[1,"date"], change.date.i - 3, change.date.i + 3)){
                            
                            description <- paste0("This plan change is highly likely to have caused the observed change in lead and conversion rate.")
                            
                        } else {
                            description <- paste0("Both lead volume and conversion rate were down, although no particular plan changes occurred that could be traced back to. See Lead Volume of Top Correlating Plans below for possibile causes.")
                        }
                    }
                    
                } else{
                    description <- paste0("No plan changes were found relating to this impact, and conversion rate either stayed relatively the same or went up while leads went down. Therefore the impact can be traced back to a change in traffic in this utility zone.")
                }
                
                description
                
            })
            
            
            
            
            
            output$plan_name9.tab <- renderText({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                leads <- get.causal.impact.data()[[7]]
                
                
                k <- 9
                
                change.date.i <- top.impacts.leads[k, "graph.date"]
                
                leads %>%
                    mutate(n = 1) %>%
                    unite(original_plan_plus_utility_id, original_plan_id, utility_id, sep = "-") %>%
                    aggregate(n~original_plan_plus_utility_id + date_submitted, data=., sum) -> leads.by.plan_plus_utility_id     
                
                lbp.spread <- spread(leads.by.plan_plus_utility_id,key = original_plan_plus_utility_id, value = n, fill = 0)
                
                lbp.zoo <- zoo(lbp.spread[,-1], lbp.spread[,1])
                
                merge(lbp.zoo, zoo(1, seq(today() - daysback, today(), by = 1)), all = TRUE) %>% 
                    .[,-(ncol(.))] %>%
                    fortify.zoo(.) %>%
                    reshape2::melt("Index") %>%
                    dplyr::rename(date_submitted = Index, original_plan_plus_utility_id = variable, n = value) %>%
                    separate(original_plan_plus_utility_id, into = c("original_plan_id", "utility_id"), sep = "-") -> lbp 
                
                
                
                
                
                til.i <- top.impacts.leads[k,]
                ticr.i <- top.impacts.cr[k,]
                
                if(between(til.i$Average_Relative_Effect, ticr.i$Avg_Rel_Eff_Lo_90_Conf_Interval, ticr.i$Avg_Rel_Eff_Up_90_Conf_Interval)){
                    
                    
                    leads.ct[leads.ct$utility_name_id == top.impacts.leads[k, "utility_name_id"],] %>%
                        filter(product == top.impacts.leads[k, "product"]) %>%
                        .[,!names(.) %in% c("utility_name_id", "product")] -> leads.i
                    
                    lbp %>%
                        filter(utility_id == top.impacts.leads[k, "utility_id"]) %>%
                        .[,!names(.) %in% c("utility_id")] %>%
                        spread(key = original_plan_id, value = n, fill = 0) -> lbp.i
                    
                    for.cor.i <- merge(leads.i, lbp.i, by = "date_submitted")
                    
                    arrange(reshape2::melt(cor(for.cor.i$n, for.cor.i[,-1:-2])),desc(value)) %>%
                        .[,!names(.) %in% c("Var1")] %>%
                        dplyr::rename(original_plan_id = Var2, cor = value) -> cor.i
                    
                    lbp.i %>%
                        .[,names(.) %in% c("date_submitted", cor.i$original_plan_id[1:5])] %>%
                        reshape2::melt("date_submitted") %>%
                        dplyr::rename(original_plan_id = variable, n = value) -> top.lbp
                    
                    changes %>%
                        filter(utility_name_id == top.impacts.leads[k, "utility_name_id"]) %>%
                        filter(original_plan_id == as.numeric(cor.i$original_plan_id[1])) -> changes.i
                    
                    changes.i <- arrange(changes.i[!duplicated(changes.i),], plan_audit_id)
                    
                    if(dim(changes.i)[1] == 0){
                        
                        paste0("Multiple Possibilities")
                        
                    } else {
                        if(between(changes.i[1,"date"], change.date.i - 3, change.date.i + 3)){
                            
                            as.character(changes.i[1, c("plan_name")])
                            
                        } else {
                            paste0("Multiple Possibilities")
                        }
                        
                    }
                    
                } else{
                    paste0("Traffic Change")
                }
                
                
                
            })
            
            
            output$plan_term9.tab <- renderText({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                leads <- get.causal.impact.data()[[7]]
                
                
                k <- 9
                
                change.date.i <- top.impacts.leads[k, "graph.date"]
                
                leads %>%
                    mutate(n = 1) %>%
                    unite(original_plan_plus_utility_id, original_plan_id, utility_id, sep = "-") %>%
                    aggregate(n~original_plan_plus_utility_id + date_submitted, data=., sum) -> leads.by.plan_plus_utility_id     
                
                lbp.spread <- spread(leads.by.plan_plus_utility_id,key = original_plan_plus_utility_id, value = n, fill = 0)
                
                lbp.zoo <- zoo(lbp.spread[,-1], lbp.spread[,1])
                
                merge(lbp.zoo, zoo(1, seq(today() - daysback, today(), by = 1)), all = TRUE) %>% 
                    .[,-(ncol(.))] %>%
                    fortify.zoo(.) %>%
                    reshape2::melt("Index") %>%
                    dplyr::rename(date_submitted = Index, original_plan_plus_utility_id = variable, n = value) %>%
                    separate(original_plan_plus_utility_id, into = c("original_plan_id", "utility_id"), sep = "-") -> lbp 
                
                
                
                til.i <- top.impacts.leads[k,]
                ticr.i <- top.impacts.cr[k,]
                
                if(between(til.i$Average_Relative_Effect, ticr.i$Avg_Rel_Eff_Lo_90_Conf_Interval, ticr.i$Avg_Rel_Eff_Up_90_Conf_Interval)){
                    
                    
                    leads.ct[leads.ct$utility_name_id == top.impacts.leads[k, "utility_name_id"],] %>%
                        filter(product == top.impacts.leads[k, "product"]) %>%
                        .[,!names(.) %in% c("utility_name_id", "product")] -> leads.i
                    
                    lbp %>%
                        filter(utility_id == top.impacts.leads[k, "utility_id"]) %>%
                        .[,!names(.) %in% c("utility_id")] %>%
                        spread(key = original_plan_id, value = n, fill = 0) -> lbp.i
                    
                    for.cor.i <- merge(leads.i, lbp.i, by = "date_submitted")
                    
                    arrange(reshape2::melt(cor(for.cor.i$n, for.cor.i[,-1:-2])),desc(value)) %>%
                        .[,!names(.) %in% c("Var1")] %>%
                        dplyr::rename(original_plan_id = Var2, cor = value) -> cor.i
                    
                    lbp.i %>%
                        .[,names(.) %in% c("date_submitted", cor.i$original_plan_id[1:5])] %>%
                        reshape2::melt("date_submitted") %>%
                        dplyr::rename(original_plan_id = variable, n = value) -> top.lbp
                    
                    changes %>%
                        filter(utility_name_id == top.impacts.leads[k, "utility_name_id"]) %>%
                        filter(original_plan_id == as.numeric(cor.i$original_plan_id[1])) -> changes.i
                    
                    changes.i <- arrange(changes.i[!duplicated(changes.i),], plan_audit_id)
                    
                    if(dim(changes.i)[1] == 0){
                        
                        paste0(" ")
                        
                    } else {
                        if(between(changes.i[1,"date"], change.date.i - 3, change.date.i + 3)){
                            
                            as.character(paste(changes.i[1, c("contract_term")], " Months ", changes.i[1, c("contract_type")], sep = ""))
                            
                        } else {
                            paste0(" ")
                        }
                        
                    }
                    
                } else{
                    paste0(" ")
                }
                
            }) 
            
            
            output$plan_supplier9.tab <- renderText({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                leads <- get.causal.impact.data()[[7]]
                
                
                k <- 9
                
                change.date.i <- top.impacts.leads[k, "graph.date"]
                
                leads %>%
                    mutate(n = 1) %>%
                    unite(original_plan_plus_utility_id, original_plan_id, utility_id, sep = "-") %>%
                    aggregate(n~original_plan_plus_utility_id + date_submitted, data=., sum) -> leads.by.plan_plus_utility_id     
                
                lbp.spread <- spread(leads.by.plan_plus_utility_id,key = original_plan_plus_utility_id, value = n, fill = 0)
                
                lbp.zoo <- zoo(lbp.spread[,-1], lbp.spread[,1])
                
                merge(lbp.zoo, zoo(1, seq(today() - daysback, today(), by = 1)), all = TRUE) %>% 
                    .[,-(ncol(.))] %>%
                    fortify.zoo(.) %>%
                    reshape2::melt("Index") %>%
                    dplyr::rename(date_submitted = Index, original_plan_plus_utility_id = variable, n = value) %>%
                    separate(original_plan_plus_utility_id, into = c("original_plan_id", "utility_id"), sep = "-") -> lbp 
                
                
                
                til.i <- top.impacts.leads[k,]
                ticr.i <- top.impacts.cr[k,]
                
                if(between(til.i$Average_Relative_Effect, ticr.i$Avg_Rel_Eff_Lo_90_Conf_Interval, ticr.i$Avg_Rel_Eff_Up_90_Conf_Interval)){
                    
                    
                    leads.ct[leads.ct$utility_name_id == top.impacts.leads[k, "utility_name_id"],] %>%
                        filter(product == top.impacts.leads[k, "product"]) %>%
                        .[,!names(.) %in% c("utility_name_id", "product")] -> leads.i
                    
                    lbp %>%
                        filter(utility_id == top.impacts.leads[k, "utility_id"]) %>%
                        .[,!names(.) %in% c("utility_id")] %>%
                        spread(key = original_plan_id, value = n, fill = 0) -> lbp.i
                    
                    for.cor.i <- merge(leads.i, lbp.i, by = "date_submitted")
                    
                    arrange(reshape2::melt(cor(for.cor.i$n, for.cor.i[,-1:-2])),desc(value)) %>%
                        .[,!names(.) %in% c("Var1")] %>%
                        dplyr::rename(original_plan_id = Var2, cor = value) -> cor.i
                    
                    lbp.i %>%
                        .[,names(.) %in% c("date_submitted", cor.i$original_plan_id[1:5])] %>%
                        reshape2::melt("date_submitted") %>%
                        dplyr::rename(original_plan_id = variable, n = value) -> top.lbp
                    
                    changes %>%
                        filter(utility_name_id == top.impacts.leads[k, "utility_name_id"]) %>%
                        filter(original_plan_id == as.numeric(cor.i$original_plan_id[1])) -> changes.i
                    
                    changes.i <- arrange(changes.i[!duplicated(changes.i),], plan_audit_id)
                    
                    if(dim(changes.i)[1] == 0){
                        
                        paste0(" ")
                        
                    } else {
                        if(between(changes.i[1,"date"], change.date.i - 3, change.date.i + 3)){
                            
                            as.character(changes.i[1, c("supplier")])
                            
                        } else {
                            paste0(" ")
                        }
                        
                    }
                    
                } else{
                    paste0(" ")
                }
                
            })
            
            
            output$plan_active9.tab <- renderText({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                leads <- get.causal.impact.data()[[7]]
                
                
                k <- 9
                
                change.date.i <- top.impacts.leads[k, "graph.date"]
                
                leads %>%
                    mutate(n = 1) %>%
                    unite(original_plan_plus_utility_id, original_plan_id, utility_id, sep = "-") %>%
                    aggregate(n~original_plan_plus_utility_id + date_submitted, data=., sum) -> leads.by.plan_plus_utility_id     
                
                lbp.spread <- spread(leads.by.plan_plus_utility_id,key = original_plan_plus_utility_id, value = n, fill = 0)
                
                lbp.zoo <- zoo(lbp.spread[,-1], lbp.spread[,1])
                
                merge(lbp.zoo, zoo(1, seq(today() - daysback, today(), by = 1)), all = TRUE) %>% 
                    .[,-(ncol(.))] %>%
                    fortify.zoo(.) %>%
                    reshape2::melt("Index") %>%
                    dplyr::rename(date_submitted = Index, original_plan_plus_utility_id = variable, n = value) %>%
                    separate(original_plan_plus_utility_id, into = c("original_plan_id", "utility_id"), sep = "-") -> lbp 
                
                
                
                til.i <- top.impacts.leads[k,]
                ticr.i <- top.impacts.cr[k,]
                
                if(between(til.i$Average_Relative_Effect, ticr.i$Avg_Rel_Eff_Lo_90_Conf_Interval, ticr.i$Avg_Rel_Eff_Up_90_Conf_Interval)){
                    
                    
                    leads.ct[leads.ct$utility_name_id == top.impacts.leads[k, "utility_name_id"],] %>%
                        filter(product == top.impacts.leads[k, "product"]) %>%
                        .[,!names(.) %in% c("utility_name_id", "product")] -> leads.i
                    
                    lbp %>%
                        filter(utility_id == top.impacts.leads[k, "utility_id"]) %>%
                        .[,!names(.) %in% c("utility_id")] %>%
                        spread(key = original_plan_id, value = n, fill = 0) -> lbp.i
                    
                    for.cor.i <- merge(leads.i, lbp.i, by = "date_submitted")
                    
                    arrange(reshape2::melt(cor(for.cor.i$n, for.cor.i[,-1:-2])),desc(value)) %>%
                        .[,!names(.) %in% c("Var1")] %>%
                        dplyr::rename(original_plan_id = Var2, cor = value) -> cor.i
                    
                    lbp.i %>%
                        .[,names(.) %in% c("date_submitted", cor.i$original_plan_id[1:5])] %>%
                        reshape2::melt("date_submitted") %>%
                        dplyr::rename(original_plan_id = variable, n = value) -> top.lbp
                    
                    changes %>%
                        filter(utility_name_id == top.impacts.leads[k, "utility_name_id"]) %>%
                        filter(original_plan_id == as.numeric(cor.i$original_plan_id[1])) -> changes.i
                    
                    changes.i <- arrange(changes.i[!duplicated(changes.i),], plan_audit_id)
                    
                    if(dim(changes.i)[1] == 0){
                        
                        paste0(" ")
                        
                    } else {
                        if(between(changes.i[1,"date"], change.date.i - 3, change.date.i + 3)){
                            
                            new_plan_status = changes.i[changes.i$plan_audit_id == max(changes.i$plan_audit_id), "plan_status_name"]
                            
                            new_plan_status
                            
                        } else {
                            paste0(" ")
                        }
                        
                    }
                    
                } else{
                    paste0(" ")
                }
                
            })
            
            output$plan_id9.tab <- renderText({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                leads <- get.causal.impact.data()[[7]]
                
                
                k <- 9
                
                change.date.i <- top.impacts.leads[k, "graph.date"]
                
                leads %>%
                    mutate(n = 1) %>%
                    unite(original_plan_plus_utility_id, original_plan_id, utility_id, sep = "-") %>%
                    aggregate(n~original_plan_plus_utility_id + date_submitted, data=., sum) -> leads.by.plan_plus_utility_id     
                
                lbp.spread <- spread(leads.by.plan_plus_utility_id,key = original_plan_plus_utility_id, value = n, fill = 0)
                
                lbp.zoo <- zoo(lbp.spread[,-1], lbp.spread[,1])
                
                merge(lbp.zoo, zoo(1, seq(today() - daysback, today(), by = 1)), all = TRUE) %>% 
                    .[,-(ncol(.))] %>%
                    fortify.zoo(.) %>%
                    reshape2::melt("Index") %>%
                    dplyr::rename(date_submitted = Index, original_plan_plus_utility_id = variable, n = value) %>%
                    separate(original_plan_plus_utility_id, into = c("original_plan_id", "utility_id"), sep = "-") -> lbp 
                
                
                
                til.i <- top.impacts.leads[k,]
                ticr.i <- top.impacts.cr[k,]
                
                if(between(til.i$Average_Relative_Effect, ticr.i$Avg_Rel_Eff_Lo_90_Conf_Interval, ticr.i$Avg_Rel_Eff_Up_90_Conf_Interval)){
                    
                    
                    leads.ct[leads.ct$utility_name_id == top.impacts.leads[k, "utility_name_id"],] %>%
                        filter(product == top.impacts.leads[k, "product"]) %>%
                        .[,!names(.) %in% c("utility_name_id", "product")] -> leads.i
                    
                    lbp %>%
                        filter(utility_id == top.impacts.leads[k, "utility_id"]) %>%
                        .[,!names(.) %in% c("utility_id")] %>%
                        spread(key = original_plan_id, value = n, fill = 0) -> lbp.i
                    
                    for.cor.i <- merge(leads.i, lbp.i, by = "date_submitted")
                    
                    arrange(reshape2::melt(cor(for.cor.i$n, for.cor.i[,-1:-2])),desc(value)) %>%
                        .[,!names(.) %in% c("Var1")] %>%
                        dplyr::rename(original_plan_id = Var2, cor = value) -> cor.i
                    
                    lbp.i %>%
                        .[,names(.) %in% c("date_submitted", cor.i$original_plan_id[1:5])] %>%
                        reshape2::melt("date_submitted") %>%
                        dplyr::rename(original_plan_id = variable, n = value) -> top.lbp
                    
                    changes %>%
                        filter(utility_name_id == top.impacts.leads[k, "utility_name_id"]) %>%
                        filter(original_plan_id == as.numeric(cor.i$original_plan_id[1])) -> changes.i
                    
                    changes.i <- arrange(changes.i[!duplicated(changes.i),], plan_audit_id)
                    
                    
                    if(dim(changes.i)[1] == 0){
                        
                        paste0(" ")
                        
                    } else {
                        if(between(changes.i[1,"date"], change.date.i - 3, change.date.i + 3)){
                            
                            as.character(paste("Plan ID: ", changes.i[1, c("original_plan_id")], sep = ""))
                            
                        } else {
                            paste0(" ")
                        }
                        
                    }
                    
                } else{
                    paste0(" ")
                }
                
                
            })
            
            
            
            output$price_change9 <- renderText({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                leads <- get.causal.impact.data()[[7]]
                
                
                k <- 9
                
                change.date.i <- top.impacts.leads[k, "graph.date"]
                
                leads %>%
                    mutate(n = 1) %>%
                    unite(original_plan_plus_utility_id, original_plan_id, utility_id, sep = "-") %>%
                    aggregate(n~original_plan_plus_utility_id + date_submitted, data=., sum) -> leads.by.plan_plus_utility_id     
                
                lbp.spread <- spread(leads.by.plan_plus_utility_id,key = original_plan_plus_utility_id, value = n, fill = 0)
                
                lbp.zoo <- zoo(lbp.spread[,-1], lbp.spread[,1])
                
                merge(lbp.zoo, zoo(1, seq(today() - daysback, today(), by = 1)), all = TRUE) %>% 
                    .[,-(ncol(.))] %>%
                    fortify.zoo(.) %>%
                    reshape2::melt("Index") %>%
                    dplyr::rename(date_submitted = Index, original_plan_plus_utility_id = variable, n = value) %>%
                    separate(original_plan_plus_utility_id, into = c("original_plan_id", "utility_id"), sep = "-") -> lbp 
                
                
                
                til.i <- top.impacts.leads[k,]
                ticr.i <- top.impacts.cr[k,]
                
                if(between(til.i$Average_Relative_Effect, ticr.i$Avg_Rel_Eff_Lo_90_Conf_Interval, ticr.i$Avg_Rel_Eff_Up_90_Conf_Interval)){
                    
                    
                    leads.ct[leads.ct$utility_name_id == top.impacts.leads[k, "utility_name_id"],] %>%
                        filter(product == top.impacts.leads[k, "product"]) %>%
                        .[,!names(.) %in% c("utility_name_id", "product")] -> leads.i
                    
                    lbp %>%
                        filter(utility_id == top.impacts.leads[k, "utility_id"]) %>%
                        .[,!names(.) %in% c("utility_id")] %>%
                        spread(key = original_plan_id, value = n, fill = 0) -> lbp.i
                    
                    for.cor.i <- merge(leads.i, lbp.i, by = "date_submitted")
                    
                    arrange(reshape2::melt(cor(for.cor.i$n, for.cor.i[,-1:-2])),desc(value)) %>%
                        .[,!names(.) %in% c("Var1")] %>%
                        dplyr::rename(original_plan_id = Var2, cor = value) -> cor.i
                    
                    lbp.i %>%
                        .[,names(.) %in% c("date_submitted", cor.i$original_plan_id[1:5])] %>%
                        reshape2::melt("date_submitted") %>%
                        dplyr::rename(original_plan_id = variable, n = value) -> top.lbp
                    
                    changes %>%
                        filter(utility_name_id == top.impacts.leads[k, "utility_name_id"]) %>%
                        filter(original_plan_id == as.numeric(cor.i$original_plan_id[1])) -> changes.i
                    
                    changes.i <- arrange(changes.i[!duplicated(changes.i),], plan_audit_id)
                    
                    
                    
                    if(dim(changes.i)[1] == 0){
                        
                        paste0(" ")
                        
                    } else {
                        if(between(changes.i[1,"date"], change.date.i - 3, change.date.i + 3)){
                            
                            old_price = changes.i[changes.i$plan_audit_id == min(changes.i$plan_audit_id), "plan_price"]
                            new_price = changes.i[changes.i$plan_audit_id == max(changes.i$plan_audit_id), "plan_price"]
                            
                            paste(old_price, "¢", " -> ", new_price, "¢", sep = "")
                            
                        } else {
                            paste0(" ")
                        }
                        
                    }
                    
                } else{
                    paste0(" ")
                }
                
                
            })
            
            
            
            
            
            
            output$change.9 <- renderInfoBox({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                leads <- get.causal.impact.data()[[7]]
                
                k <- 9
                
                change.date.i <- max(changes[changes$change_index == top.impacts.leads$change_index[k], "date"])
                
                product.i <- unique(changes[changes$change_index == top.impacts.leads$change_index[k], "product"])
                utility.i <- paste(unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility"]), unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility_id"]), sep = " - ")
                
                leads.ct %>%
                    filter(utility_name_id == utility.i) %>%
                    filter(product == product.i) -> leads.i
                
                leads.ts <- zoo(leads.i[,!names(leads.i) %in% c("date_submitted", "utility_name_id", "product")], order.by = leads.i$date_submitted)
                pre.i <- c(min(leads.i$date_submitted), change.date.i)
                post.i <- c(change.date.i + 1, max(leads.i$date_submitted))
                impact.i <- try.Causal.Impact(time_series = leads.ts, pre_date = pre.i, post_date = post.i, conf_int = 0.05)
                
                if(impact.i$summary$AbsEffect[1] < 0){
                    subt <- "Lead Volume Decline"
                    ico <- icon("arrow-down")
                } else {
                    subt <- "Lead Volume Increase"
                    ico <- icon("arrow-up")
                }
                
                infoBox(
                    "What occurred:", subt, icon = ico,
                    color = "purple"
                )
                
            })
            
            output$utility.9 <- renderInfoBox({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                
                k <- 9
                infoBox(
                    "Where:", paste(top.impacts.leads[k, c("state")], top.impacts.leads[k, c("utility")], sep = " - "), icon = icon("plug", "fa-0.5x"),
                    color = "green"
                )        
            })
            
            output$product.9 <- renderInfoBox({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                
                k <- 9
                if(top.impacts.leads[k, c("product")] == "Electricity"){
                    ico <- icon("bolt", "fa-0.5x")
                } else{
                    ico <- icon("fire", "fa-0.5x")
                }
                infoBox(
                    "Which product:", top.impacts.leads[k, c("product")], icon = ico,
                    color = "yellow"
                )        
            })
            
            
            output$causal.impact.cr.plot9 <- renderPlot({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                
                changes.unique <- changes[!duplicated(changes[,c("date", "utility_id")]),]
                unique.index <- unique(changes.unique$change_index)
                
                k <- 9
                
                change.date.i <- max(changes[changes$change_index == top.impacts.leads$change_index[k], "date"])
                
                product.i <- unique(changes[changes$change_index == top.impacts.leads$change_index[k], "product"])
                utility.i <- paste(unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility"]), unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility_id"]), sep = " - ")
                
                cr.ct %>%
                    filter(utility_name_id == utility.i) %>%
                    filter(product == product.i) -> leads.i
                
                leads.ts <- zoo(leads.i[,!names(leads.i) %in% c("date_submitted", "utility_name_id", "product")], order.by = leads.i$date_submitted)
                pre.i <- c(min(leads.i$date_submitted), change.date.i)
                post.i <- c(change.date.i + 1, max(leads.i$date_submitted))
                impact.i <- try.Causal.Impact(time_series = leads.ts, pre_date = pre.i, post_date = post.i, conf_int = 0.05)
                plot(impact.i)
            })
            
            
            output$causal.impact.cr.summary.EXCLUDE <- renderPrint({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                
                changes.unique <- changes[!duplicated(changes[,c("date", "utility_id")]),]
                unique.index <- unique(changes.unique$change_index)
                
                k <- 9
                
                change.date.i <- max(changes[changes$change_index == top.impacts.leads$change_index[k], "date"])
                
                product.i <- unique(changes[changes$change_index == top.impacts.leads$change_index[k], "product"])
                utility.i <- paste(unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility"]), unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility_id"]), sep = " - ")
                
                cr.ct %>%
                    filter(utility_name_id == utility.i) %>%
                    filter(product == product.i) -> leads.i
                
                leads.ts <- zoo(leads.i[,!names(leads.i) %in% c("date_submitted", "utility_name_id", "product")], order.by = leads.i$date_submitted)
                pre.i <- c(min(leads.i$date_submitted), change.date.i)
                post.i <- c(change.date.i + 1, max(leads.i$date_submitted))
                impact.i <- try.Causal.Impact(time_series = leads.ts, pre_date = pre.i, post_date = post.i, conf_int = 0.05)
                summary(impact.i)
            })        
            
            
            output$causal.impact.cr.summary9 <- renderPrint({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                
                changes.unique <- changes[!duplicated(changes[,c("date", "utility_id")]),]
                unique.index <- unique(changes.unique$change_index)
                
                k <- 9
                
                change.date.i <- max(changes[changes$change_index == top.impacts.leads$change_index[k], "date"])
                
                product.i <- unique(changes[changes$change_index == top.impacts.leads$change_index[k], "product"])
                utility.i <- paste(unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility"]), unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility_id"]), sep = " - ")
                cr.ct %>%
                    filter(utility_name_id == utility.i) %>%
                    filter(product == product.i) -> leads.i
                
                leads.ts <- zoo(leads.i[,!names(leads.i) %in% c("date_submitted", "utility_name_id", "product")], order.by = leads.i$date_submitted)
                pre.i <- c(min(leads.i$date_submitted), change.date.i)
                post.i <- c(change.date.i + 1, max(leads.i$date_submitted))
                impact.i <- try.Causal.Impact(time_series = leads.ts, pre_date = pre.i, post_date = post.i, conf_int = 0.05)
                summary(impact.i)
            })
            
            output$causal.impact.leads.summary9 <- renderPrint({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                
                changes.unique <- changes[!duplicated(changes[,c("date", "utility_id")]),]
                unique.index <- unique(changes.unique$change_index)
                
                k <- 9
                
                change.date.i <- max(changes[changes$change_index == top.impacts.leads$change_index[k], "date"])
                
                product.i <- unique(changes[changes$change_index == top.impacts.leads$change_index[k], "product"])
                utility.i <- paste(unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility"]), unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility_id"]), sep = " - ")
                leads.ct %>%
                    filter(utility_name_id == utility.i) %>%
                    filter(product == product.i) -> leads.i
                
                leads.ts <- zoo(leads.i[,!names(leads.i) %in% c("date_submitted", "utility_name_id", "product")], order.by = leads.i$date_submitted)
                pre.i <- c(min(leads.i$date_submitted), change.date.i)
                post.i <- c(change.date.i + 1, max(leads.i$date_submitted))
                impact.i <- try.Causal.Impact(time_series = leads.ts, pre_date = pre.i, post_date = post.i, conf_int = 0.05)
                summary(impact.i)
            })
            
            
            output$causal.impact.leads.plot9 <- renderPlot({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                
                changes.unique <- changes[!duplicated(changes[,c("date", "utility_id")]),]
                unique.index <- unique(changes.unique$change_index)
                
                k <- 9
                
                change.date.i <- max(changes[changes$change_index == top.impacts.leads$change_index[k], "date"])
                
                product.i <- unique(changes[changes$change_index == top.impacts.leads$change_index[k], "product"])
                utility.i <- paste(unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility"]), unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility_id"]), sep = " - ")
                leads.ct %>%
                    filter(utility_name_id == utility.i) %>%
                    filter(product == product.i) -> leads.i
                
                leads.ts <- zoo(leads.i[,!names(leads.i) %in% c("date_submitted", "utility_name_id", "product")], order.by = leads.i$date_submitted)
                pre.i <- c(min(leads.i$date_submitted), change.date.i)
                post.i <- c(change.date.i + 1, max(leads.i$date_submitted))
                impact.i <- try.Causal.Impact(time_series = leads.ts, pre_date = pre.i, post_date = post.i, conf_int = 0.05)
                plot(impact.i)
            })
            
            
            output$complete.plan.changes9 <- renderPlotly({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                
                changes.unique <- changes[!duplicated(changes[,c("date", "utility_id")]),]
                unique.index <- unique(changes.unique$change_index)
                
                k <- 9
                
                change.date.i <- max(changes[changes$change_index == top.impacts.leads$change_index[k], "date"])
                
                product.i <- unique(changes[changes$change_index == top.impacts.leads$change_index[k], "product"])
                utility.i <- paste(unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility"]), unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility_id"]), sep = " - ")
                
                utility.name.i <- unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility_name_id"])
                changes[changes$utility_name_id == utility.name.i, ] %>%
                    filter(graph.date > (today()-(daysback+1))) %>%
                    mutate(Supplier_Plan = paste(change, supplier, plan_name, sep = " - ")) -> changes.full.i
                
                daily.prices %>%
                    filter(utility_name_id == utility.i) %>%
                    filter(product == product.i) -> daily.prices.i
                
                
                
                p <- ggplot() + 
                    geom_line(data = daily.prices.i, aes(date, plan_price.mean)) +
                    geom_line(data = daily.prices.i, aes(date, plan_price.max)) +
                    geom_line(data = daily.prices.i, aes(date, plan_price.min)) +
                    geom_line(data = changes.full.i[changes.full.i$change == "plan_change",], aes(graph.date, plan_price, col = Supplier_Plan)) +
                    geom_point(data = changes.full.i[changes.full.i$change == "plan_end",], aes(graph.date, plan_price, col = Supplier_Plan)) + 
                    geom_point(data = changes.full.i[changes.full.i$change == "new_plan",], aes(graph.date, plan_price, col = Supplier_Plan)) +
                    geom_vline(xintercept = as.numeric(change.date.i), linetype = "longdash") +
                    ggtitle(paste("All Plan Changes Around", change.date.i, sep = " ")) +
                    xlab("Date") +
                    ylab("Plan Price (cents)") +
                    theme(legend.position = "none", panel.background=element_blank(), panel.grid.major = element_line(colour = "grey"))
                
                ggplotly(p)
            })
            
            
            
            
            output$plan_breakdown9 <- renderPlot({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                leads <- get.causal.impact.data()[[7]]
                sessions.ct <- get.causal.impact.data()[[8]]     
                plan.inv <- get.causal.impact.data()[[9]]
                
                
                changes.unique <- changes[!duplicated(changes[,c("date", "utility_id")]),]
                unique.index <- unique(changes.unique$change_index)
                
                k <- 9
                
                change.date.i <- max(changes[changes$change_index == top.impacts.leads$change_index[k], "date"])
                
                product.i <- unique(changes[changes$change_index == top.impacts.leads$change_index[k], "product"])
                utility.i <- paste(unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility"]), unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility_id"]), sep = " - ")
                
                utility.name.i <- unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility_name_id"])
                changes[changes$utility_name_id == utility.name.i, ] %>%
                    filter(graph.date > (today()-(daysback+1))) %>%
                    mutate(suplan = paste(change, supplier, plan_name, sep = " - ")) -> changes.full.i
                
                daily.prices %>%
                    filter(utility_name_id == utility.i) %>%
                    filter(product == product.i) -> daily.prices.i
                
                
                leads %>%
                    mutate(n = 1) %>%
                    unite(original_plan_plus_utility_id, original_plan_id, utility_id, sep = "-") %>%
                    aggregate(n~original_plan_plus_utility_id + date_submitted, data=., sum) -> leads.by.plan_plus_utility_id     
                
                lbp.spread <- spread(leads.by.plan_plus_utility_id,key = original_plan_plus_utility_id, value = n, fill = 0)
                
                lbp.zoo <- zoo(lbp.spread[,-1], lbp.spread[,1])
                
                merge(lbp.zoo, zoo(1, seq(today() - daysback, today(), by = 1)), all = TRUE) %>% 
                    .[,-(ncol(.))] %>%
                    fortify.zoo(.) %>%
                    reshape2::melt("Index") %>%
                    dplyr::rename(date_submitted = Index, original_plan_plus_utility_id = variable, n = value) %>%
                    separate(original_plan_plus_utility_id, into = c("original_plan_id", "utility_id"), sep = "-") -> lbp 
                
                
                
                til.i <- top.impacts.leads[k,]
                ticr.i <- top.impacts.cr[k,]
                
                if(between(til.i$Average_Relative_Effect, ticr.i$Avg_Rel_Eff_Lo_90_Conf_Interval, ticr.i$Avg_Rel_Eff_Up_90_Conf_Interval)){
                    
                    
                    leads.ct[leads.ct$utility_name_id == top.impacts.leads[k, "utility_name_id"],] %>%
                        filter(product == top.impacts.leads[k, "product"]) %>%
                        .[,!names(.) %in% c("utility_name_id", "product")] -> leads.i
                    
                    lbp %>%
                        filter(utility_id == top.impacts.leads[k, "utility_id"]) %>%
                        .[,!names(.) %in% c("utility_id")] %>%
                        spread(key = original_plan_id, value = n, fill = 0) -> lbp.i
                    
                    for.cor.i <- merge(leads.i, lbp.i, by = "date_submitted")
                    
                    arrange(reshape2::melt(cor(for.cor.i$n, for.cor.i[,-1:-2])),desc(value)) %>%
                        .[,!names(.) %in% c("Var1")] %>%
                        dplyr::rename(original_plan_id = Var2, cor = value) -> cor.i
                    
                    lbp.i %>%
                        .[,names(.) %in% c("date_submitted", cor.i$original_plan_id[1:5])] %>%
                        reshape2::melt("date_submitted") %>%
                        dplyr::rename(original_plan_id = variable, n = value) %>%
                        merge(unique(plan.inv[,c("supplier", "plan_name", "original_plan_id")]), by = "original_plan_id", all.x = TRUE) %>%
                        unite(suplan, supplier, plan_name, sep = " - ", remove = TRUE) %>%
                        .[,!names(.) %in% c("original_plan_id")] -> top.lbp
                    
                    changes %>%
                        filter(utility_name_id == top.impacts.leads[k, "utility_name_id"]) %>%
                        filter(original_plan_id == as.numeric(cor.i$original_plan_id[1])) -> changes.i
                    
                    changes.i <- arrange(changes.i[!duplicated(changes.i),], plan_audit_id)
                    
                    leads.i %>%
                        mutate(suplan = "Total Leads") %>%
                        .[,c("date_submitted", "n", "suplan")] %>%
                        rbind(top.lbp) -> leads.for.plot
                    
                    leads.for.plot$suplan <- as.character(leads.for.plot$suplan)
                    leads.for.plot$suplan <- factor(leads.for.plot$suplan, levels = c("Total Leads", unique(top.lbp$suplan)))
                    
                    names(leads.for.plot) <- c("date_submitted", "n", "suplan")
                    
                    ggplot(leads.for.plot, aes(date_submitted, n)) + 
                        geom_line() + 
                        facet_wrap(~suplan, ncol = 1) + 
                        ggtitle("Lead Volume of Top Correlating Plans") +
                        theme(legend.position = "none", panel.background=element_blank(), panel.grid.major = element_line(colour = "grey"), strip.text.y = element_text(angle=0), plot.title = element_text(hjust = 0.5, size = 18))+
                        xlab("Date") +
                        ylab("Lead Volume") +
                        geom_vline(xintercept = as.numeric(change.date.i), linetype = "longdash")
                    
                } else{
                    sessions.ct %>%
                        filter(utility_name_id == top.impacts.leads[k, "utility_name_id"]) %>%
                        aggregate(uniquePageviews ~ date_submitted, data=., sum) -> sessions.for.plot
                    
                    ggplot(sessions.for.plot, aes(date_submitted, uniquePageviews)) + 
                        geom_line() + 
                        ggtitle(paste("Sessions in ", top.impacts.leads[k, "utility"], sep = "")) +
                        theme(legend.position = "none", panel.background=element_blank(), panel.grid.major = element_line(colour = "grey")) +
                        xlab("Date") +
                        ylab("Sessions") +
                        geom_vline(xintercept = as.numeric(change.date.i), linetype = "longdash") +
                        theme(plot.title = element_text(hjust = 0.5, size = 18)) +
                        stat_smooth(method=loess)
                    
                }
                
                
            })
            
            
            # # # --- # # # --- # # # --- # # # --- # # # ---
            
            #                      TAB 10                    #  
            
            # # # --- # # # --- # # # --- # # # --- # # # ---
            
            
            
            
            
            
            
            output$likelihood.description.10 <- renderText({
                
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                leads <- get.causal.impact.data()[[7]]
                
                
                k <- 10
                
                change.date.i <- top.impacts.leads[k, "graph.date"]
                
                leads %>%
                    mutate(n = 1) %>%
                    unite(original_plan_plus_utility_id, original_plan_id, utility_id, sep = "-") %>%
                    aggregate(n~original_plan_plus_utility_id + date_submitted, data=., sum) -> leads.by.plan_plus_utility_id     
                
                lbp.spread <- spread(leads.by.plan_plus_utility_id,key = original_plan_plus_utility_id, value = n, fill = 0)
                
                lbp.zoo <- zoo(lbp.spread[,-1], lbp.spread[,1])
                
                merge(lbp.zoo, zoo(1, seq(today() - daysback, today(), by = 1)), all = TRUE) %>% 
                    .[,-(ncol(.))] %>%
                    fortify.zoo(.) %>%
                    reshape2::melt("Index") %>%
                    dplyr::rename(date_submitted = Index, original_plan_plus_utility_id = variable, n = value) %>%
                    separate(original_plan_plus_utility_id, into = c("original_plan_id", "utility_id"), sep = "-") -> lbp 
                
                
                til.i <- top.impacts.leads[k,]
                ticr.i <- top.impacts.cr[k,]
                
                if(between(til.i$Average_Relative_Effect, ticr.i$Avg_Rel_Eff_Lo_90_Conf_Interval, ticr.i$Avg_Rel_Eff_Up_90_Conf_Interval)){
                    
                    
                    leads.ct[leads.ct$utility_name_id == top.impacts.leads[k, "utility_name_id"],] %>%
                        filter(product == top.impacts.leads[k, "product"]) %>%
                        .[,!names(.) %in% c("utility_name_id", "product")] -> leads.i
                    
                    lbp %>%
                        filter(utility_id == top.impacts.leads[k, "utility_id"]) %>%
                        .[,!names(.) %in% c("utility_id")] %>%
                        spread(key = original_plan_id, value = n, fill = 0) -> lbp.i
                    
                    for.cor.i <- merge(leads.i, lbp.i, by = "date_submitted")
                    
                    arrange(reshape2::melt(cor(for.cor.i$n, for.cor.i[,-1:-2])),desc(value)) %>%
                        .[,!names(.) %in% c("Var1")] %>%
                        dplyr::rename(original_plan_id = Var2, cor = value) -> cor.i
                    
                    lbp.i %>%
                        .[,names(.) %in% c("date_submitted", cor.i$original_plan_id[1:5])] %>%
                        reshape2::melt("date_submitted") %>%
                        dplyr::rename(original_plan_id = variable, n = value) -> top.lbp
                    
                    changes %>%
                        filter(utility_name_id == top.impacts.leads[k, "utility_name_id"]) %>%
                        filter(original_plan_id == as.numeric(cor.i$original_plan_id[1])) -> changes.i
                    
                    changes.i <- arrange(changes.i[!duplicated(changes.i),], plan_audit_id)
                    
                    if(dim(changes.i)[1] == 0){
                        
                        description <- paste0("Both lead volume and conversion rate were down, although no particular plan changes occurred that could be traced back to. See Lead Volume of Top Correlating Plans below for possibile causes.")
                        
                    } else {
                        if(between(changes.i[1,"date"], change.date.i - 3, change.date.i + 3)){
                            
                            description <- paste0("This plan change is highly likely to have caused the observed change in lead and conversion rate.")
                            
                        } else {
                            description <- paste0("Both lead volume and conversion rate were down, although no particular plan changes occurred that could be traced back to. See Lead Volume of Top Correlating Plans below for possibile causes.")
                        }
                    }
                    
                } else{
                    description <- paste0("No plan changes were found relating to this impact, and conversion rate either stayed relatively the same or went up while leads went down. Therefore the impact can be traced back to a change in traffic in this utility zone.")
                }
                
                description
                
            })
            
            
            
            
            
            output$plan_name10.tab <- renderText({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                leads <- get.causal.impact.data()[[7]]
                
                
                k <- 10
                
                change.date.i <- top.impacts.leads[k, "graph.date"]
                
                leads %>%
                    mutate(n = 1) %>%
                    unite(original_plan_plus_utility_id, original_plan_id, utility_id, sep = "-") %>%
                    aggregate(n~original_plan_plus_utility_id + date_submitted, data=., sum) -> leads.by.plan_plus_utility_id     
                
                lbp.spread <- spread(leads.by.plan_plus_utility_id,key = original_plan_plus_utility_id, value = n, fill = 0)
                
                lbp.zoo <- zoo(lbp.spread[,-1], lbp.spread[,1])
                
                merge(lbp.zoo, zoo(1, seq(today() - daysback, today(), by = 1)), all = TRUE) %>% 
                    .[,-(ncol(.))] %>%
                    fortify.zoo(.) %>%
                    reshape2::melt("Index") %>%
                    dplyr::rename(date_submitted = Index, original_plan_plus_utility_id = variable, n = value) %>%
                    separate(original_plan_plus_utility_id, into = c("original_plan_id", "utility_id"), sep = "-") -> lbp 
                
                
                
                
                
                til.i <- top.impacts.leads[k,]
                ticr.i <- top.impacts.cr[k,]
                
                if(between(til.i$Average_Relative_Effect, ticr.i$Avg_Rel_Eff_Lo_90_Conf_Interval, ticr.i$Avg_Rel_Eff_Up_90_Conf_Interval)){
                    
                    
                    leads.ct[leads.ct$utility_name_id == top.impacts.leads[k, "utility_name_id"],] %>%
                        filter(product == top.impacts.leads[k, "product"]) %>%
                        .[,!names(.) %in% c("utility_name_id", "product")] -> leads.i
                    
                    lbp %>%
                        filter(utility_id == top.impacts.leads[k, "utility_id"]) %>%
                        .[,!names(.) %in% c("utility_id")] %>%
                        spread(key = original_plan_id, value = n, fill = 0) -> lbp.i
                    
                    for.cor.i <- merge(leads.i, lbp.i, by = "date_submitted")
                    
                    arrange(reshape2::melt(cor(for.cor.i$n, for.cor.i[,-1:-2])),desc(value)) %>%
                        .[,!names(.) %in% c("Var1")] %>%
                        dplyr::rename(original_plan_id = Var2, cor = value) -> cor.i
                    
                    lbp.i %>%
                        .[,names(.) %in% c("date_submitted", cor.i$original_plan_id[1:5])] %>%
                        reshape2::melt("date_submitted") %>%
                        dplyr::rename(original_plan_id = variable, n = value) -> top.lbp
                    
                    changes %>%
                        filter(utility_name_id == top.impacts.leads[k, "utility_name_id"]) %>%
                        filter(original_plan_id == as.numeric(cor.i$original_plan_id[1])) -> changes.i
                    
                    changes.i <- arrange(changes.i[!duplicated(changes.i),], plan_audit_id)
                    
                    if(dim(changes.i)[1] == 0){
                        
                        paste0("Multiple Possibilities")
                        
                    } else {
                        if(between(changes.i[1,"date"], change.date.i - 3, change.date.i + 3)){
                            
                            as.character(changes.i[1, c("plan_name")])
                            
                        } else {
                            paste0("Multiple Possibilities")
                        }
                        
                    }
                    
                } else{
                    paste0("Traffic Change")
                }
                
                
                
            })
            
            
            output$plan_term10.tab <- renderText({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                leads <- get.causal.impact.data()[[7]]
                
                
                k <- 10
                
                change.date.i <- top.impacts.leads[k, "graph.date"]
                
                leads %>%
                    mutate(n = 1) %>%
                    unite(original_plan_plus_utility_id, original_plan_id, utility_id, sep = "-") %>%
                    aggregate(n~original_plan_plus_utility_id + date_submitted, data=., sum) -> leads.by.plan_plus_utility_id     
                
                lbp.spread <- spread(leads.by.plan_plus_utility_id,key = original_plan_plus_utility_id, value = n, fill = 0)
                
                lbp.zoo <- zoo(lbp.spread[,-1], lbp.spread[,1])
                
                merge(lbp.zoo, zoo(1, seq(today() - daysback, today(), by = 1)), all = TRUE) %>% 
                    .[,-(ncol(.))] %>%
                    fortify.zoo(.) %>%
                    reshape2::melt("Index") %>%
                    dplyr::rename(date_submitted = Index, original_plan_plus_utility_id = variable, n = value) %>%
                    separate(original_plan_plus_utility_id, into = c("original_plan_id", "utility_id"), sep = "-") -> lbp 
                
                
                
                til.i <- top.impacts.leads[k,]
                ticr.i <- top.impacts.cr[k,]
                
                if(between(til.i$Average_Relative_Effect, ticr.i$Avg_Rel_Eff_Lo_90_Conf_Interval, ticr.i$Avg_Rel_Eff_Up_90_Conf_Interval)){
                    
                    
                    leads.ct[leads.ct$utility_name_id == top.impacts.leads[k, "utility_name_id"],] %>%
                        filter(product == top.impacts.leads[k, "product"]) %>%
                        .[,!names(.) %in% c("utility_name_id", "product")] -> leads.i
                    
                    lbp %>%
                        filter(utility_id == top.impacts.leads[k, "utility_id"]) %>%
                        .[,!names(.) %in% c("utility_id")] %>%
                        spread(key = original_plan_id, value = n, fill = 0) -> lbp.i
                    
                    for.cor.i <- merge(leads.i, lbp.i, by = "date_submitted")
                    
                    arrange(reshape2::melt(cor(for.cor.i$n, for.cor.i[,-1:-2])),desc(value)) %>%
                        .[,!names(.) %in% c("Var1")] %>%
                        dplyr::rename(original_plan_id = Var2, cor = value) -> cor.i
                    
                    lbp.i %>%
                        .[,names(.) %in% c("date_submitted", cor.i$original_plan_id[1:5])] %>%
                        reshape2::melt("date_submitted") %>%
                        dplyr::rename(original_plan_id = variable, n = value) -> top.lbp
                    
                    changes %>%
                        filter(utility_name_id == top.impacts.leads[k, "utility_name_id"]) %>%
                        filter(original_plan_id == as.numeric(cor.i$original_plan_id[1])) -> changes.i
                    
                    changes.i <- arrange(changes.i[!duplicated(changes.i),], plan_audit_id)
                    
                    if(dim(changes.i)[1] == 0){
                        
                        paste0(" ")
                        
                    } else {
                        if(between(changes.i[1,"date"], change.date.i - 3, change.date.i + 3)){
                            
                            as.character(paste(changes.i[1, c("contract_term")], " Months ", changes.i[1, c("contract_type")], sep = ""))
                            
                        } else {
                            paste0(" ")
                        }
                        
                    }
                    
                } else{
                    paste0(" ")
                }
                
            }) 
            
            
            output$plan_supplier10.tab <- renderText({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                leads <- get.causal.impact.data()[[7]]
                
                
                k <- 10
                
                change.date.i <- top.impacts.leads[k, "graph.date"]
                
                leads %>%
                    mutate(n = 1) %>%
                    unite(original_plan_plus_utility_id, original_plan_id, utility_id, sep = "-") %>%
                    aggregate(n~original_plan_plus_utility_id + date_submitted, data=., sum) -> leads.by.plan_plus_utility_id     
                
                lbp.spread <- spread(leads.by.plan_plus_utility_id,key = original_plan_plus_utility_id, value = n, fill = 0)
                
                lbp.zoo <- zoo(lbp.spread[,-1], lbp.spread[,1])
                
                merge(lbp.zoo, zoo(1, seq(today() - daysback, today(), by = 1)), all = TRUE) %>% 
                    .[,-(ncol(.))] %>%
                    fortify.zoo(.) %>%
                    reshape2::melt("Index") %>%
                    dplyr::rename(date_submitted = Index, original_plan_plus_utility_id = variable, n = value) %>%
                    separate(original_plan_plus_utility_id, into = c("original_plan_id", "utility_id"), sep = "-") -> lbp 
                
                
                
                til.i <- top.impacts.leads[k,]
                ticr.i <- top.impacts.cr[k,]
                
                if(between(til.i$Average_Relative_Effect, ticr.i$Avg_Rel_Eff_Lo_90_Conf_Interval, ticr.i$Avg_Rel_Eff_Up_90_Conf_Interval)){
                    
                    
                    leads.ct[leads.ct$utility_name_id == top.impacts.leads[k, "utility_name_id"],] %>%
                        filter(product == top.impacts.leads[k, "product"]) %>%
                        .[,!names(.) %in% c("utility_name_id", "product")] -> leads.i
                    
                    lbp %>%
                        filter(utility_id == top.impacts.leads[k, "utility_id"]) %>%
                        .[,!names(.) %in% c("utility_id")] %>%
                        spread(key = original_plan_id, value = n, fill = 0) -> lbp.i
                    
                    for.cor.i <- merge(leads.i, lbp.i, by = "date_submitted")
                    
                    arrange(reshape2::melt(cor(for.cor.i$n, for.cor.i[,-1:-2])),desc(value)) %>%
                        .[,!names(.) %in% c("Var1")] %>%
                        dplyr::rename(original_plan_id = Var2, cor = value) -> cor.i
                    
                    lbp.i %>%
                        .[,names(.) %in% c("date_submitted", cor.i$original_plan_id[1:5])] %>%
                        reshape2::melt("date_submitted") %>%
                        dplyr::rename(original_plan_id = variable, n = value) -> top.lbp
                    
                    changes %>%
                        filter(utility_name_id == top.impacts.leads[k, "utility_name_id"]) %>%
                        filter(original_plan_id == as.numeric(cor.i$original_plan_id[1])) -> changes.i
                    
                    changes.i <- arrange(changes.i[!duplicated(changes.i),], plan_audit_id)
                    
                    if(dim(changes.i)[1] == 0){
                        
                        paste0(" ")
                        
                    } else {
                        if(between(changes.i[1,"date"], change.date.i - 3, change.date.i + 3)){
                            
                            as.character(changes.i[1, c("supplier")])
                            
                        } else {
                            paste0(" ")
                        }
                        
                    }
                    
                } else{
                    paste0(" ")
                }
                
            })
            
            
            output$plan_active10.tab <- renderText({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                leads <- get.causal.impact.data()[[7]]
                
                
                k <- 10
                
                change.date.i <- top.impacts.leads[k, "graph.date"]
                
                leads %>%
                    mutate(n = 1) %>%
                    unite(original_plan_plus_utility_id, original_plan_id, utility_id, sep = "-") %>%
                    aggregate(n~original_plan_plus_utility_id + date_submitted, data=., sum) -> leads.by.plan_plus_utility_id     
                
                lbp.spread <- spread(leads.by.plan_plus_utility_id,key = original_plan_plus_utility_id, value = n, fill = 0)
                
                lbp.zoo <- zoo(lbp.spread[,-1], lbp.spread[,1])
                
                merge(lbp.zoo, zoo(1, seq(today() - daysback, today(), by = 1)), all = TRUE) %>% 
                    .[,-(ncol(.))] %>%
                    fortify.zoo(.) %>%
                    reshape2::melt("Index") %>%
                    dplyr::rename(date_submitted = Index, original_plan_plus_utility_id = variable, n = value) %>%
                    separate(original_plan_plus_utility_id, into = c("original_plan_id", "utility_id"), sep = "-") -> lbp 
                
                
                
                til.i <- top.impacts.leads[k,]
                ticr.i <- top.impacts.cr[k,]
                
                if(between(til.i$Average_Relative_Effect, ticr.i$Avg_Rel_Eff_Lo_90_Conf_Interval, ticr.i$Avg_Rel_Eff_Up_90_Conf_Interval)){
                    
                    
                    leads.ct[leads.ct$utility_name_id == top.impacts.leads[k, "utility_name_id"],] %>%
                        filter(product == top.impacts.leads[k, "product"]) %>%
                        .[,!names(.) %in% c("utility_name_id", "product")] -> leads.i
                    
                    lbp %>%
                        filter(utility_id == top.impacts.leads[k, "utility_id"]) %>%
                        .[,!names(.) %in% c("utility_id")] %>%
                        spread(key = original_plan_id, value = n, fill = 0) -> lbp.i
                    
                    for.cor.i <- merge(leads.i, lbp.i, by = "date_submitted")
                    
                    arrange(reshape2::melt(cor(for.cor.i$n, for.cor.i[,-1:-2])),desc(value)) %>%
                        .[,!names(.) %in% c("Var1")] %>%
                        dplyr::rename(original_plan_id = Var2, cor = value) -> cor.i
                    
                    lbp.i %>%
                        .[,names(.) %in% c("date_submitted", cor.i$original_plan_id[1:5])] %>%
                        reshape2::melt("date_submitted") %>%
                        dplyr::rename(original_plan_id = variable, n = value) -> top.lbp
                    
                    changes %>%
                        filter(utility_name_id == top.impacts.leads[k, "utility_name_id"]) %>%
                        filter(original_plan_id == as.numeric(cor.i$original_plan_id[1])) -> changes.i
                    
                    changes.i <- arrange(changes.i[!duplicated(changes.i),], plan_audit_id)
                    
                    if(dim(changes.i)[1] == 0){
                        
                        paste0(" ")
                        
                    } else {
                        if(between(changes.i[1,"date"], change.date.i - 3, change.date.i + 3)){
                            
                            new_plan_status = changes.i[changes.i$plan_audit_id == max(changes.i$plan_audit_id), "plan_status_name"]
                            
                            new_plan_status
                            
                        } else {
                            paste0(" ")
                        }
                        
                    }
                    
                } else{
                    paste0(" ")
                }
                
            })
            
            output$plan_id10.tab <- renderText({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                leads <- get.causal.impact.data()[[7]]
                
                
                k <- 10
                
                change.date.i <- top.impacts.leads[k, "graph.date"]
                
                leads %>%
                    mutate(n = 1) %>%
                    unite(original_plan_plus_utility_id, original_plan_id, utility_id, sep = "-") %>%
                    aggregate(n~original_plan_plus_utility_id + date_submitted, data=., sum) -> leads.by.plan_plus_utility_id     
                
                lbp.spread <- spread(leads.by.plan_plus_utility_id,key = original_plan_plus_utility_id, value = n, fill = 0)
                
                lbp.zoo <- zoo(lbp.spread[,-1], lbp.spread[,1])
                
                merge(lbp.zoo, zoo(1, seq(today() - daysback, today(), by = 1)), all = TRUE) %>% 
                    .[,-(ncol(.))] %>%
                    fortify.zoo(.) %>%
                    reshape2::melt("Index") %>%
                    dplyr::rename(date_submitted = Index, original_plan_plus_utility_id = variable, n = value) %>%
                    separate(original_plan_plus_utility_id, into = c("original_plan_id", "utility_id"), sep = "-") -> lbp 
                
                
                
                til.i <- top.impacts.leads[k,]
                ticr.i <- top.impacts.cr[k,]
                
                if(between(til.i$Average_Relative_Effect, ticr.i$Avg_Rel_Eff_Lo_90_Conf_Interval, ticr.i$Avg_Rel_Eff_Up_90_Conf_Interval)){
                    
                    
                    leads.ct[leads.ct$utility_name_id == top.impacts.leads[k, "utility_name_id"],] %>%
                        filter(product == top.impacts.leads[k, "product"]) %>%
                        .[,!names(.) %in% c("utility_name_id", "product")] -> leads.i
                    
                    lbp %>%
                        filter(utility_id == top.impacts.leads[k, "utility_id"]) %>%
                        .[,!names(.) %in% c("utility_id")] %>%
                        spread(key = original_plan_id, value = n, fill = 0) -> lbp.i
                    
                    for.cor.i <- merge(leads.i, lbp.i, by = "date_submitted")
                    
                    arrange(reshape2::melt(cor(for.cor.i$n, for.cor.i[,-1:-2])),desc(value)) %>%
                        .[,!names(.) %in% c("Var1")] %>%
                        dplyr::rename(original_plan_id = Var2, cor = value) -> cor.i
                    
                    lbp.i %>%
                        .[,names(.) %in% c("date_submitted", cor.i$original_plan_id[1:5])] %>%
                        reshape2::melt("date_submitted") %>%
                        dplyr::rename(original_plan_id = variable, n = value) -> top.lbp
                    
                    changes %>%
                        filter(utility_name_id == top.impacts.leads[k, "utility_name_id"]) %>%
                        filter(original_plan_id == as.numeric(cor.i$original_plan_id[1])) -> changes.i
                    
                    changes.i <- arrange(changes.i[!duplicated(changes.i),], plan_audit_id)
                    
                    
                    if(dim(changes.i)[1] == 0){
                        
                        paste0(" ")
                        
                    } else {
                        if(between(changes.i[1,"date"], change.date.i - 3, change.date.i + 3)){
                            
                            as.character(paste("Plan ID: ", changes.i[1, c("original_plan_id")], sep = ""))
                            
                        } else {
                            paste0(" ")
                        }
                        
                    }
                    
                } else{
                    paste0(" ")
                }
                
                
            })
            
            
            
            output$price_change10 <- renderText({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                leads <- get.causal.impact.data()[[7]]
                
                
                k <- 10
                
                change.date.i <- top.impacts.leads[k, "graph.date"]
                
                leads %>%
                    mutate(n = 1) %>%
                    unite(original_plan_plus_utility_id, original_plan_id, utility_id, sep = "-") %>%
                    aggregate(n~original_plan_plus_utility_id + date_submitted, data=., sum) -> leads.by.plan_plus_utility_id     
                
                lbp.spread <- spread(leads.by.plan_plus_utility_id,key = original_plan_plus_utility_id, value = n, fill = 0)
                
                lbp.zoo <- zoo(lbp.spread[,-1], lbp.spread[,1])
                
                merge(lbp.zoo, zoo(1, seq(today() - daysback, today(), by = 1)), all = TRUE) %>% 
                    .[,-(ncol(.))] %>%
                    fortify.zoo(.) %>%
                    reshape2::melt("Index") %>%
                    dplyr::rename(date_submitted = Index, original_plan_plus_utility_id = variable, n = value) %>%
                    separate(original_plan_plus_utility_id, into = c("original_plan_id", "utility_id"), sep = "-") -> lbp 
                
                
                
                til.i <- top.impacts.leads[k,]
                ticr.i <- top.impacts.cr[k,]
                
                if(between(til.i$Average_Relative_Effect, ticr.i$Avg_Rel_Eff_Lo_90_Conf_Interval, ticr.i$Avg_Rel_Eff_Up_90_Conf_Interval)){
                    
                    
                    leads.ct[leads.ct$utility_name_id == top.impacts.leads[k, "utility_name_id"],] %>%
                        filter(product == top.impacts.leads[k, "product"]) %>%
                        .[,!names(.) %in% c("utility_name_id", "product")] -> leads.i
                    
                    lbp %>%
                        filter(utility_id == top.impacts.leads[k, "utility_id"]) %>%
                        .[,!names(.) %in% c("utility_id")] %>%
                        spread(key = original_plan_id, value = n, fill = 0) -> lbp.i
                    
                    for.cor.i <- merge(leads.i, lbp.i, by = "date_submitted")
                    
                    arrange(reshape2::melt(cor(for.cor.i$n, for.cor.i[,-1:-2])),desc(value)) %>%
                        .[,!names(.) %in% c("Var1")] %>%
                        dplyr::rename(original_plan_id = Var2, cor = value) -> cor.i
                    
                    lbp.i %>%
                        .[,names(.) %in% c("date_submitted", cor.i$original_plan_id[1:5])] %>%
                        reshape2::melt("date_submitted") %>%
                        dplyr::rename(original_plan_id = variable, n = value) -> top.lbp
                    
                    changes %>%
                        filter(utility_name_id == top.impacts.leads[k, "utility_name_id"]) %>%
                        filter(original_plan_id == as.numeric(cor.i$original_plan_id[1])) -> changes.i
                    
                    changes.i <- arrange(changes.i[!duplicated(changes.i),], plan_audit_id)
                    
                    
                    
                    if(dim(changes.i)[1] == 0){
                        
                        paste0(" ")
                        
                    } else {
                        if(between(changes.i[1,"date"], change.date.i - 3, change.date.i + 3)){
                            
                            old_price = changes.i[changes.i$plan_audit_id == min(changes.i$plan_audit_id), "plan_price"]
                            new_price = changes.i[changes.i$plan_audit_id == max(changes.i$plan_audit_id), "plan_price"]
                            
                            paste(old_price, "¢", " -> ", new_price, "¢", sep = "")
                            
                        } else {
                            paste0(" ")
                        }
                        
                    }
                    
                } else{
                    paste0(" ")
                }
                
                
            })
            
            
            
            
            
            
            output$change.10 <- renderInfoBox({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                leads <- get.causal.impact.data()[[7]]
                
                k <- 10
                
                change.date.i <- max(changes[changes$change_index == top.impacts.leads$change_index[k], "date"])
                
                product.i <- unique(changes[changes$change_index == top.impacts.leads$change_index[k], "product"])
                utility.i <- paste(unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility"]), unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility_id"]), sep = " - ")
                
                leads.ct %>%
                    filter(utility_name_id == utility.i) %>%
                    filter(product == product.i) -> leads.i
                
                leads.ts <- zoo(leads.i[,!names(leads.i) %in% c("date_submitted", "utility_name_id", "product")], order.by = leads.i$date_submitted)
                pre.i <- c(min(leads.i$date_submitted), change.date.i)
                post.i <- c(change.date.i + 1, max(leads.i$date_submitted))
                impact.i <- try.Causal.Impact(time_series = leads.ts, pre_date = pre.i, post_date = post.i, conf_int = 0.05)
                
                if(impact.i$summary$AbsEffect[1] < 0){
                    subt <- "Lead Volume Decline"
                    ico <- icon("arrow-down")
                } else {
                    subt <- "Lead Volume Increase"
                    ico <- icon("arrow-up")
                }
                
                infoBox(
                    "What occurred:", subt, icon = ico,
                    color = "purple"
                )
                
            })
            
            output$utility.10 <- renderInfoBox({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                
                k <- 10
                infoBox(
                    "Where:", paste(top.impacts.leads[k, c("state")], top.impacts.leads[k, c("utility")], sep = " - "), icon = icon("plug", "fa-0.5x"),
                    color = "green"
                )        
            })
            
            output$product.10 <- renderInfoBox({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                
                k <- 10
                if(top.impacts.leads[k, c("product")] == "Electricity"){
                    ico <- icon("bolt", "fa-0.5x")
                } else{
                    ico <- icon("fire", "fa-0.5x")
                }
                infoBox(
                    "Which product:", top.impacts.leads[k, c("product")], icon = ico,
                    color = "yellow"
                )        
            })
            
            
            output$causal.impact.cr.plot10 <- renderPlot({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                
                changes.unique <- changes[!duplicated(changes[,c("date", "utility_id")]),]
                unique.index <- unique(changes.unique$change_index)
                
                k <- 10
                
                change.date.i <- max(changes[changes$change_index == top.impacts.leads$change_index[k], "date"])
                
                product.i <- unique(changes[changes$change_index == top.impacts.leads$change_index[k], "product"])
                utility.i <- paste(unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility"]), unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility_id"]), sep = " - ")
                
                cr.ct %>%
                    filter(utility_name_id == utility.i) %>%
                    filter(product == product.i) -> leads.i
                
                leads.ts <- zoo(leads.i[,!names(leads.i) %in% c("date_submitted", "utility_name_id", "product")], order.by = leads.i$date_submitted)
                pre.i <- c(min(leads.i$date_submitted), change.date.i)
                post.i <- c(change.date.i + 1, max(leads.i$date_submitted))
                impact.i <- try.Causal.Impact(time_series = leads.ts, pre_date = pre.i, post_date = post.i, conf_int = 0.05)
                plot(impact.i)
            })
            
            
            output$causal.impact.cr.summary.EXCLUDE <- renderPrint({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                
                changes.unique <- changes[!duplicated(changes[,c("date", "utility_id")]),]
                unique.index <- unique(changes.unique$change_index)
                
                k <- 10
                
                change.date.i <- max(changes[changes$change_index == top.impacts.leads$change_index[k], "date"])
                
                product.i <- unique(changes[changes$change_index == top.impacts.leads$change_index[k], "product"])
                utility.i <- paste(unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility"]), unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility_id"]), sep = " - ")
                
                cr.ct %>%
                    filter(utility_name_id == utility.i) %>%
                    filter(product == product.i) -> leads.i
                
                leads.ts <- zoo(leads.i[,!names(leads.i) %in% c("date_submitted", "utility_name_id", "product")], order.by = leads.i$date_submitted)
                pre.i <- c(min(leads.i$date_submitted), change.date.i)
                post.i <- c(change.date.i + 1, max(leads.i$date_submitted))
                impact.i <- try.Causal.Impact(time_series = leads.ts, pre_date = pre.i, post_date = post.i, conf_int = 0.05)
                summary(impact.i)
            })        
            
            
            output$causal.impact.cr.summary10 <- renderPrint({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                
                changes.unique <- changes[!duplicated(changes[,c("date", "utility_id")]),]
                unique.index <- unique(changes.unique$change_index)
                
                k <- 10
                
                change.date.i <- max(changes[changes$change_index == top.impacts.leads$change_index[k], "date"])
                
                product.i <- unique(changes[changes$change_index == top.impacts.leads$change_index[k], "product"])
                utility.i <- paste(unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility"]), unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility_id"]), sep = " - ")
                cr.ct %>%
                    filter(utility_name_id == utility.i) %>%
                    filter(product == product.i) -> leads.i
                
                leads.ts <- zoo(leads.i[,!names(leads.i) %in% c("date_submitted", "utility_name_id", "product")], order.by = leads.i$date_submitted)
                pre.i <- c(min(leads.i$date_submitted), change.date.i)
                post.i <- c(change.date.i + 1, max(leads.i$date_submitted))
                impact.i <- try.Causal.Impact(time_series = leads.ts, pre_date = pre.i, post_date = post.i, conf_int = 0.05)
                summary(impact.i)
            })
            
            output$causal.impact.leads.summary10 <- renderPrint({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                
                changes.unique <- changes[!duplicated(changes[,c("date", "utility_id")]),]
                unique.index <- unique(changes.unique$change_index)
                
                k <- 10
                
                change.date.i <- max(changes[changes$change_index == top.impacts.leads$change_index[k], "date"])
                
                product.i <- unique(changes[changes$change_index == top.impacts.leads$change_index[k], "product"])
                utility.i <- paste(unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility"]), unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility_id"]), sep = " - ")
                leads.ct %>%
                    filter(utility_name_id == utility.i) %>%
                    filter(product == product.i) -> leads.i
                
                leads.ts <- zoo(leads.i[,!names(leads.i) %in% c("date_submitted", "utility_name_id", "product")], order.by = leads.i$date_submitted)
                pre.i <- c(min(leads.i$date_submitted), change.date.i)
                post.i <- c(change.date.i + 1, max(leads.i$date_submitted))
                impact.i <- try.Causal.Impact(time_series = leads.ts, pre_date = pre.i, post_date = post.i, conf_int = 0.05)
                summary(impact.i)
            })
            
            
            output$causal.impact.leads.plot10 <- renderPlot({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                
                changes.unique <- changes[!duplicated(changes[,c("date", "utility_id")]),]
                unique.index <- unique(changes.unique$change_index)
                
                k <- 10
                
                change.date.i <- max(changes[changes$change_index == top.impacts.leads$change_index[k], "date"])
                
                product.i <- unique(changes[changes$change_index == top.impacts.leads$change_index[k], "product"])
                utility.i <- paste(unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility"]), unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility_id"]), sep = " - ")
                leads.ct %>%
                    filter(utility_name_id == utility.i) %>%
                    filter(product == product.i) -> leads.i
                
                leads.ts <- zoo(leads.i[,!names(leads.i) %in% c("date_submitted", "utility_name_id", "product")], order.by = leads.i$date_submitted)
                pre.i <- c(min(leads.i$date_submitted), change.date.i)
                post.i <- c(change.date.i + 1, max(leads.i$date_submitted))
                impact.i <- try.Causal.Impact(time_series = leads.ts, pre_date = pre.i, post_date = post.i, conf_int = 0.05)
                plot(impact.i)
            })
            
            
            output$complete.plan.changes10 <- renderPlotly({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                
                changes.unique <- changes[!duplicated(changes[,c("date", "utility_id")]),]
                unique.index <- unique(changes.unique$change_index)
                
                k <- 10
                
                change.date.i <- max(changes[changes$change_index == top.impacts.leads$change_index[k], "date"])
                
                product.i <- unique(changes[changes$change_index == top.impacts.leads$change_index[k], "product"])
                utility.i <- paste(unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility"]), unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility_id"]), sep = " - ")
                
                utility.name.i <- unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility_name_id"])
                changes[changes$utility_name_id == utility.name.i, ] %>%
                    filter(graph.date > (today()-(daysback+1))) %>%
                    mutate(Supplier_Plan = paste(change, supplier, plan_name, sep = " - ")) -> changes.full.i
                
                daily.prices %>%
                    filter(utility_name_id == utility.i) %>%
                    filter(product == product.i) -> daily.prices.i
                
                p <- ggplot() + 
                    geom_line(data = daily.prices.i, aes(date, plan_price.mean)) +
                    geom_line(data = daily.prices.i, aes(date, plan_price.max)) +
                    geom_line(data = daily.prices.i, aes(date, plan_price.min)) +
                    geom_line(data = changes.full.i[changes.full.i$change == "plan_change",], aes(graph.date, plan_price, col = Supplier_Plan)) +
                    geom_point(data = changes.full.i[changes.full.i$change == "plan_end",], aes(graph.date, plan_price, col = Supplier_Plan)) + 
                    geom_point(data = changes.full.i[changes.full.i$change == "new_plan",], aes(graph.date, plan_price, col = Supplier_Plan)) +
                    geom_vline(xintercept = as.numeric(change.date.i), linetype = "longdash") +
                    ggtitle(paste("All Plan Changes Around", change.date.i, sep = " ")) +
                    xlab("Date") +
                    ylab("Plan Price (cents)") +
                    theme(legend.position = "none", panel.background=element_blank(), panel.grid.major = element_line(colour = "grey"))
                
                ggplotly(p)
            })
            
            
            
            
            
            output$plan_breakdown10 <- renderPlot({
                daily.prices <- get.causal.impact.data()[[1]]
                changes <- get.causal.impact.data()[[2]]
                leads.ct <- get.causal.impact.data()[[3]]
                cr.ct <- get.causal.impact.data()[[4]]
                top.impacts.leads <- get.causal.impact.data()[[5]]
                top.impacts.cr <- get.causal.impact.data()[[6]]
                leads <- get.causal.impact.data()[[7]]
                sessions.ct <- get.causal.impact.data()[[8]]     
                plan.inv <- get.causal.impact.data()[[9]]
                
                
                changes.unique <- changes[!duplicated(changes[,c("date", "utility_id")]),]
                unique.index <- unique(changes.unique$change_index)
                
                k <- 10
                
                change.date.i <- max(changes[changes$change_index == top.impacts.leads$change_index[k], "date"])
                
                product.i <- unique(changes[changes$change_index == top.impacts.leads$change_index[k], "product"])
                utility.i <- paste(unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility"]), unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility_id"]), sep = " - ")
                
                utility.name.i <- unique(changes[changes$change_index == top.impacts.leads$change_index[k], "utility_name_id"])
                changes[changes$utility_name_id == utility.name.i, ] %>%
                    filter(graph.date > (today()-(daysback+1))) %>%
                    mutate(suplan = paste(change, supplier, plan_name, sep = " - ")) -> changes.full.i
                
                daily.prices %>%
                    filter(utility_name_id == utility.i) %>%
                    filter(product == product.i) -> daily.prices.i
                
                
                leads %>%
                    mutate(n = 1) %>%
                    unite(original_plan_plus_utility_id, original_plan_id, utility_id, sep = "-") %>%
                    aggregate(n~original_plan_plus_utility_id + date_submitted, data=., sum) -> leads.by.plan_plus_utility_id     
                
                lbp.spread <- spread(leads.by.plan_plus_utility_id,key = original_plan_plus_utility_id, value = n, fill = 0)
                
                lbp.zoo <- zoo(lbp.spread[,-1], lbp.spread[,1])
                
                merge(lbp.zoo, zoo(1, seq(today() - daysback, today(), by = 1)), all = TRUE) %>% 
                    .[,-(ncol(.))] %>%
                    fortify.zoo(.) %>%
                    reshape2::melt("Index") %>%
                    dplyr::rename(date_submitted = Index, original_plan_plus_utility_id = variable, n = value) %>%
                    separate(original_plan_plus_utility_id, into = c("original_plan_id", "utility_id"), sep = "-") -> lbp 
                
                
                
                til.i <- top.impacts.leads[k,]
                ticr.i <- top.impacts.cr[k,]
                
                if(between(til.i$Average_Relative_Effect, ticr.i$Avg_Rel_Eff_Lo_90_Conf_Interval, ticr.i$Avg_Rel_Eff_Up_90_Conf_Interval)){
                    
                    
                    leads.ct[leads.ct$utility_name_id == top.impacts.leads[k, "utility_name_id"],] %>%
                        filter(product == top.impacts.leads[k, "product"]) %>%
                        .[,!names(.) %in% c("utility_name_id", "product")] -> leads.i
                    
                    lbp %>%
                        filter(utility_id == top.impacts.leads[k, "utility_id"]) %>%
                        .[,!names(.) %in% c("utility_id")] %>%
                        spread(key = original_plan_id, value = n, fill = 0) -> lbp.i
                    
                    for.cor.i <- merge(leads.i, lbp.i, by = "date_submitted")
                    
                    arrange(reshape2::melt(cor(for.cor.i$n, for.cor.i[,-1:-2])),desc(value)) %>%
                        .[,!names(.) %in% c("Var1")] %>%
                        dplyr::rename(original_plan_id = Var2, cor = value) -> cor.i
                    
                    lbp.i %>%
                        .[,names(.) %in% c("date_submitted", cor.i$original_plan_id[1:5])] %>%
                        reshape2::melt("date_submitted") %>%
                        dplyr::rename(original_plan_id = variable, n = value) %>%
                        merge(unique(plan.inv[,c("supplier", "plan_name", "original_plan_id")]), by = "original_plan_id", all.x = TRUE) %>%
                        unite(suplan, supplier, plan_name, sep = " - ", remove = TRUE) %>%
                        .[,!names(.) %in% c("original_plan_id")] -> top.lbp
                    
                    changes %>%
                        filter(utility_name_id == top.impacts.leads[k, "utility_name_id"]) %>%
                        filter(original_plan_id == as.numeric(cor.i$original_plan_id[1])) -> changes.i
                    
                    changes.i <- arrange(changes.i[!duplicated(changes.i),], plan_audit_id)
                    
                    leads.i %>%
                        mutate(suplan = "Total Leads") %>%
                        .[,c("date_submitted", "n", "suplan")] %>%
                        rbind(top.lbp) -> leads.for.plot
                    
                    leads.for.plot$suplan <- as.character(leads.for.plot$suplan)
                    leads.for.plot$suplan <- factor(leads.for.plot$suplan, levels = c("Total Leads", unique(top.lbp$suplan)))
                    
                    names(leads.for.plot) <- c("date_submitted", "n", "suplan")
                    
                    ggplot(leads.for.plot, aes(date_submitted, n)) + 
                        geom_line() + 
                        facet_wrap(~suplan, ncol = 1) + 
                        ggtitle("Lead Volume of Top Correlating Plans") +
                        theme(legend.position = "none", panel.background=element_blank(), panel.grid.major = element_line(colour = "grey"), strip.text.y = element_text(angle=0), plot.title = element_text(hjust = 0.5, size = 18))+
                        xlab("Date") +
                        ylab("Lead Volume") +
                        geom_vline(xintercept = as.numeric(change.date.i), linetype = "longdash")
                    
                } else{
                    sessions.ct %>%
                        filter(utility_name_id == top.impacts.leads[k, "utility_name_id"]) %>%
                        aggregate(uniquePageviews ~ date_submitted, data=., sum) -> sessions.for.plot
                    
                    ggplot(sessions.for.plot, aes(date_submitted, uniquePageviews)) + 
                        geom_line() + 
                        ggtitle(paste("Sessions in ", top.impacts.leads[k, "utility"], sep = "")) +
                        theme(legend.position = "none", panel.background=element_blank(), panel.grid.major = element_line(colour = "grey")) +
                        xlab("Date") +
                        ylab("Sessions") +
                        geom_vline(xintercept = as.numeric(change.date.i), linetype = "longdash") +
                        theme(plot.title = element_text(hjust = 0.5, size = 18)) +
                        stat_smooth(method=loess)
                    
                }
                
                
            })
           
            output$top.image <- renderImage({
                filename <- normalizePath(file.path('./images',
                                                    paste('Top', '.png', sep='')))
                
                list(src = filename)
                
            }, deleteFile = FALSE)
            
            
            output$middle.image <- renderImage({
                return(list(
                    src = "Middle.png",
                    contentType = "image/png"))
            })
            
            output$bottom.image <- renderImage({
                return(list(
                    src = "Bottom.png",
                    contentType = "image/png"))
            })

            
            
            
            
            
            

            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            

    # Set number of days to look back historically at impacts on lead volume and conversion rate

    daysback <- 40        
    
    
    
    
    ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
    ##       All Plan Changes Causal Impact Analysis Function         ##
    ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
    
    
    # Reactive function to gather the data, calculate the different impacts on lead volume, and predict the most likely catalyst for each
    
    
    
    get.causal.impact.data <- reactive({ 
        
        
        
        
        ### ### ### ### ### ### ### ###
        #     Get Plan Inventory      #
        ### ### ### ### ### ### ### ###
        
        
        
        
        if(launch.container == "dev"){
            con <- dbConnect(MySQL(),
                             user="XXXXXXX", password="XXXXXXXXX",
                             dbname="XXXXXX", host="XXXXXXXXXX")
        }else{
            con <- dbConnect(MySQL(),
                             user="XXXXXXXX", password="XXXXXXXXX",
                             dbname="XXXXXXXXX", host="XXXXXXXXX", port = XXXX)
        }
        
        query <- paste("select 
                       plan_utility_prices.plan_utility_price_id,
                       plan_utility_prices.plan_utility_id,
                       plans.plan_id,
                       utilities.utility_id,
                       plan_list_view.original_plan_id,
                       plan_list_view.plan_audit_id,
                       suppliers.name AS 'supplier',
                       utilities.utility_name AS 'utility',
                       products.display_name AS 'product',
                       service_types.display_name AS 'service_type',
                       plans.state,
                       plans.plan_name,
                       plans.contract_type,
                       plans.start_date,
                       plans.end_date,
                       plan_utility.active,
                       plan_list_view.original_active_date,
                       plan_utility.created AS 'plan_utility_created',
                       plan_utility.updated AS 'plan_utility_updated',
                       plans.created AS 'plans_created',
                       plans.updated AS 'plans_updated',
                       plans.contract_term,
                       plan_utility_prices.amount AS 'plan_price',
                       plan_utility_prices.name AS 'multitier pricing',
                       plan_fees.label AS 'fee_type',
                       plan_fees.amount AS 'fee_amount',
                       plan_status.plan_status_name
                       
                       from plan_utility_prices
                       
                       
                       left join plan_utility
                       on plan_utility_prices.plan_utility_id = plan_utility.plan_utility_id
                       
                       left join plans
                       on plan_utility.plan_id = plans.plan_id
                       
                       left join products
                       on plans.product_id = products.product_id
                       
                       left join state_suppliers
                       on plans.state_supplier_id = state_suppliers.state_supplier_id
                       
                       left join suppliers
                       on state_suppliers.supplier_id = suppliers.supplier_id
                       
                       left join service_types
                       on plans.service_type_id = service_types.service_type_id
                       
                       left join plan_fees
                       on plans.plan_id = plan_fees.plan_id
                       
                       left join utilities
                       on plan_utility.utility_id = utilities.utility_id
                       
                       left join plan_status
                       on plans.plan_status_id = plan_status.plan_status_id
                       
                       left join plan_list_view
                       on plans.plan_id = plan_list_view.plan_id
                       
                       where plans.end_date > (NOW() - INTERVAL ", as.numeric(daysback), " DAY) OR plans.end_date IS NULL
                       
                       order by plans.end_date desc", sep = "")
        query2 <- "set character set 'utf8'"
        
        db <- dbGetQuery(con, query2)
        db <- dbGetQuery(con, query)
        dbDisconnect(con)
        
        db$start_date <- as.Date(db$start_date)
        db$end_date <- as.Date(db$end_date)
        db$original_active_date <- as.Date(db$original_active_date)
        db$plan_utility_created <- as.Date(db$plan_utility_created)
        db$plan_utility_updated <- as.Date(db$plan_utility_updated)
        db$plans_created <- as.Date(db$plans_created)
        db$plans_updated <- as.Date(db$plans_updated)
        
        db$plan_price <- db$plan_price/1000
        
        db %>%
            mutate(realtime_end_date = as.Date(ifelse(is.na(end_date) == TRUE, today(), end_date), origin = "1970-01-01"))%>%
            filter(realtime_end_date > (today() - as.numeric(daysback))) -> ppl.plans
        
        ppl.plans$original_active_date[is.na(ppl.plans$original_active_date)] <- ppl.plans$start_date[is.na(ppl.plans$original_active_date)]
        
        
        ppl.plans %>%
            mutate(n = 1) %>%
            aggregate(n~plan_utility_price_id, data=., sum) %>% filter(n > 1) -> grape
        
        dupes <- ppl.plans[ppl.plans$plan_utility_price_id %in% grape$plan_utility_price_id, ]
        dupes.removed <- dupes[duplicated(dupes$plan_utility_price_id),]
        
        non.dupes <- ppl.plans[!ppl.plans$plan_utility_price_id %in% grape$plan_utility_price_id, ]
        
        ppl.plans <- rbind(non.dupes, dupes.removed)
        
        dates <- seq(today() - as.numeric(daysback), today(), "day")
        
        plan.inv <- c()
        #pb <- txtProgressBar(min = 0, max = length(dates), style = 3)
        withProgress(message = 'Part 1 of 6 (Approx. 5 min remain) - Retrieving Current Plan Inventory', value = 0,{
            for(i in 1:length(dates)){
                date_i <- dates[i]
                ppl.plans[ppl.plans$start_date <= date_i,] %>%
                    .[.$realtime_end_date >= date_i,] %>%
                    mutate(date = date_i) -> plans_i
                plan.inv <- rbind(plan.inv, plans_i)
                #setTxtProgressBar(pb, i)
                incProgress(i/length(dates), detail = paste("Doing part", i, "in", length(dates)))
            }
            #close(pb)
        })
        
        names(plan.inv) <- sub(" ", "_", names(plan.inv))
        
        plan.inv %>%
            filter(service_type == "Residential") -> plan.inv
        
        plan.inv %>%
            filter(multitier_pricing == "tx_500") -> tx.plans
        
        plan.inv %>%
            filter(!state == "TX") %>%
            rbind(tx.plans) %>%
            unite(original_plan_utility_id, original_plan_id, utility_id, sep = "-", remove = FALSE) -> plan.inv
        
        
        ppl.plans %>%
            aggregate(original_active_date~original_plan_id, data=., min) %>%
            dplyr::rename(actual_original_start_date = original_active_date) -> original.start.date
        
        plan.inv %>%
            left_join(original.start.date, by = "original_plan_id") -> plan.inv
        
        plan.inv[(plan.inv$product == "Natural Gas") & (plan.inv$plan_price > 200), "plan_price"] <- plan.inv[(plan.inv$product == "Natural Gas") & (plan.inv$plan_price > 200), "plan_price"]/10
        
        plan.inv[(plan.inv$product == "Natural Gas") & (plan.inv$plan_price > 200), "plan_price"] <- plan.inv[(plan.inv$product == "Natural Gas") & (plan.inv$plan_price > 200), "plan_price"]/10
        
        plan.inv %>%
            mutate(utility_name_id = paste(utility, utility_id, sep = " - ")) %>%
            aggregate(plan_price~date + utility_name_id + product, data=., FUN = function(x) c(mean = mean(x), max = max(x), min = min(x))) -> daily.prices
        
        daily.prices <- as.data.frame(as.list(daily.prices), stringsAsFactors = F)
        

    
    
        
        
        
        
        
        
        
        
            
            
            ### Discern All changes in the plans over past 'daysback' days ###
            
            
            changes <- c()
            
            utilities.for.changes <- unique(plan.inv$original_plan_utility_id)
            
            #pb <- txtProgressBar(min = 0, max = length(utilities.for.changes), style = 3)
            withProgress(message = 'Part 2 of 6 - Discerning Plan Changes', value = 0,{
            for(j in 1:length(utilities.for.changes)){
                
                original_plan_utility.j <- utilities.for.changes[j]
                
                plan.changes.all <- plan.inv[plan.inv$original_plan_utility_id == original_plan_utility.j,]
                
                
                if((max(plan.changes.all$date) < today())&&(plan.changes.all[plan.changes.all$date == max(plan.changes.all$date), "plan_status_name"] == "Inactive - System")&&(!is.na(plan.changes.all[plan.changes.all$date == max(plan.changes.all$date), "end_date"])) ){
                    plan.changes.all[plan.changes.all$date == max(plan.changes.all$date),] %>%
                        mutate(change = "plan_end") %>%
                        mutate(change_index = paste(i,j,sep = "-")) %>%
                        mutate(graph.date = date) -> plan.changes
                    
                    if(dim(plan.changes)[1] > 1){
                        plan.changes <- plan.changes[plan.changes$plan_audit_id == max(plan.changes$plan_audit_id),]
                    } else{
                    }
                } else{
                    dual.change <- arrange(plan.changes.all, plan_audit_id)
                    
                    if(dim(dual.change)[1] == 1){
                    } else {
                        for(i in 1:(dim(dual.change)[1]-1)){
                            
                            if(dual.change[i,"plan_price"] == dual.change[i+1, "plan_price"]){
                                
                            } else {
                                
                                dual.change[i:(i+1),] %>%
                                    mutate(change = "plan_change") %>%
                                    mutate(change_index = paste(i,j,sep = "-")) -> plan.changes
                                
                                plan.changes$date <- plan.changes$original_active_date[2]
                                plan.changes %>%
                                    mutate(graph.date = date) -> plan.changes
                                
                                plan.changes[1, "graph.date"] <- plan.changes[1, "graph.date"] - 1
                                #stopifnot(!any(plan.changes$date == as.Date("2015-08-22")))
                            }
                        }
                    }
                    if(dim(plan.changes.all[plan.changes.all$actual_original_start_date >= (today() - daysback),])[1] > 0){
                        
                        plan.changes.original <- plan.changes.all[plan.changes.all$actual_original_start_date >= (today() - daysback),]
                        plan.changes.original[plan.changes.original$date == min(plan.changes.original$date),] %>%
                            mutate(change = "new_plan") %>%
                            mutate(change_index = paste(i,j,sep = "-")) -> plan.changes.new.plans
                        plan.changes.new.plans <- plan.changes.new.plans[plan.changes.new.plans$plan_audit_id == min(plan.changes.new.plans$plan_audit_id),]
                        plan.changes.new.plans %>%
                            mutate(graph.date = date) -> plan.changes.new.plans
                        changes <- rbind(changes, plan.changes.new.plans)
                        
                        ind <- apply(changes[,!names(changes) %in% c("change", "change_index")], 1, function(x) all(is.na(x)))
                        stopifnot(sum(ind) == 0)
                        
                    } else {
                        plan.changes.new.plans <- 0
                    }
                    
                    if(!exists("plan.changes")) {
                      next
                    } else if(length(unique(dual.change$plan_price)) == 1){
                       next
                    } else if((dim(plan.changes)[1] == 1) && (plan.changes$change == "plan_change")) {
                        next
                    } else if (is.null(dim(plan.changes))){
                        next
                    } else {
                        if((length(unique(plan.changes$plan_price)) < 2) && (plan.changes$change == "plan_change")){
                            next
                        } else{
                            changes <- rbind(changes, plan.changes)
                        }
                    }
                    
                    
                    #}
                }
                
                #setTxtProgressBar(pb, j)
                incProgress(j/length(utilities.for.changes), detail = paste("Doing part", j, "in", length(utilities.for.changes)))
                
            }
            #close(pb)
            })
            
            
            changes %>%
                unite(utility_name_id, utility, utility_id, sep = " - ", remove = FALSE) -> changes

    
    
    

        
        
        ### ### ### ### ### ### ### ### ### ### ### ### ## 
        #       Get Lead Volume for 'daysback' Days      #
        ### ### ### ### ### ### ### ### ### ### ### ### ##
        
        
            if(launch.container == "dev"){
                con <- dbConnect(MySQL(),
                                 user="XXXXXXX", password="XXXXXXXXX",
                                 dbname="XXXXXX", host="XXXXXXXXXX")
            }else{
                con <- dbConnect(MySQL(),
                                 user="XXXXXXXX", password="XXXXXXXXX",
                                 dbname="XXXXXXXXX", host="XXXXXXXXX", port = XXXX)
            }
        
        query <- paste("select 
                       leads.lead_id,
                       consumers.first_name,
                       consumers.last_name,
                       consumers.email,
                       addresses.address1,
                       addresses.city,
                       addresses.state,
                       addresses.postal_code,
                       addresses.county,
                       leads.created,
                       leads.date_submitted,
                       suppliers.name AS 'supplier',
                       utilities.utility_name AS 'utility',
                       utilities.utility_id,
                       products.display_name AS 'product',
                       service_types.display_name AS 'service_type',
                       plans.plan_name,
                       plan_list_view.original_plan_id,
                       plans.contract_type,
                       plans.contract_term,
                       min(plan_utility_prices.amount) AS 'plan_price',
                       plan_fees.label AS 'fee_type',
                       plan_fees.description,
                       plan_fees.amount AS 'fee_amount'
                       from leads
                       
                       left join state_suppliers
                       on leads.state_supplier_id = state_suppliers.state_supplier_id
                       
                       left join suppliers
                       on state_suppliers.supplier_id = suppliers.supplier_id
                       
                       left join consumers
                       on leads.consumer_id = consumers.consumer_id
                       
                       left join addresses
                       on leads.address_id = addresses.address_id
                       
                       left join state_supplier_lead_status
                       on leads.state_supplier_lead_status_id = state_supplier_lead_status.state_supplier_lead_status_id
                       
                       left join plan_utility_prices
                       on leads.plan_utility_id = plan_utility_prices.plan_utility_id
                       
                       left join plan_utility
                       on leads.plan_utility_id = plan_utility.plan_utility_id
                       
                       left join plans
                       on plan_utility.plan_id = plans.plan_id

                       left join plan_list_view
                       on plans.plan_id = plan_list_view.plan_id
                       
                       left join products
                       on leads.product_id = products.product_id
                       
                       left join service_types
                       on leads.service_type_id = service_types.service_type_id
                       
                       left join plan_fees
                       on plans.plan_id = plan_fees.plan_id
                       
                       left join utilities
                       on plan_utility.utility_id = utilities.utility_id
                       
                       where products.display_name <> 'Solar Power' and leads.date_submitted > (NOW() - INTERVAL ", as.numeric(daysback), " DAY) AND leads.service_type_id = 1
                       
                       group by leads.lead_id
                       
                       order by date_submitted desc", sep = "")
        
        query2 <- "set character set 'utf8'"
        
        db <- dbGetQuery(con, query2)
        db <- dbGetQuery(con, query)
        dbDisconnect(con)
        
        db$date_submitted <- as.Date(db$date_submitted)
        
        db$plan_price <- db$plan_price/1000
        
        leads <- db
        
        leads %>%
            unite(utility_name_id, utility, utility_id, sep = " - ", remove = FALSE) %>%
            mutate(n = 1) %>%
            aggregate(n~utility_name_id + date_submitted + product, data=., sum) -> leads.agg
        
        expand.grid(unique(leads.agg$date_submitted), unique(leads.agg$utility_name_id), unique(leads.agg$product)) %>%
            mutate(n = 0) %>%
            dplyr::rename(date_submitted = Var1) %>%
            dplyr::rename(utility_name_id = Var2) %>%
            dplyr::rename(product = Var3) %>%
            .[,c("date_submitted", "utility_name_id", "product", "n")] -> all.poss
        
        all.poss$utility_name_id <- as.character(all.poss$utility_name_id)
        all.poss$product <- as.character(all.poss$product)
        
        puck.full <- left_join(all.poss, leads.agg, by = c("utility_name_id","date_submitted", "product"))
        
        puck.full[is.na(puck.full$n.y), c("n.y")] <- 0
        
        puck.full %>%
            mutate(n = n.x + n.y) %>%
            .[, c("date_submitted", "utility_name_id", "product", "n")] %>%
            arrange(date_submitted) -> leads.ct
 
    
          
        
        
        
        
        ### ### ### ### ### ### ### ### ### ### ### ### ### ## 
        #       Get Conversion Rate for 'daysback' Days      #
        ### ### ### ### ### ### ### ### ### ### ### ### ### ##
        
        
        # Retrieve unique pageviews to indicate traffic
        
        
        withProgress(message = 'Part 3 of 6 - Reterieving Sessions Data', value = 0,{
            
        i <- 1
        incProgress(i/2, detail = paste("Doing part", i, "in", 2))
        start <- today() - as.numeric(daysback) - 1
        end <- today() - 1
        
        ## Load token and validate ##
        token <- Auth(client.id = "XXXXXXXXX", client.secret = "XXXXXX")
        ValidateToken(token)
        
        ## Pull GA Leads Data by Channel & State##
        query.list <- Init(start.date = paste(start, sep = ""),
                           end.date = paste(end, sep = ""),
                           dimensions = "ga:date, ga:pagePathLevel4, ga:channelGrouping",
                           metrics = "ga:uniquePageviews",
                           max.results = 50000,
                           table.id = "ga:66293701",
                           filters = "ga:pagePathLevel4=~(electricity|naturalgas)")
        
        ga.query <- QueryBuilder(query.list)
        ga.sess <- GetReportData(ga.query, token, paginate_query = T)
        ga.sess <- filter(ga.sess, channelGrouping != 'Affiliate')
        ga.sess <- filter(ga.sess, channelGrouping != 'Call Center')
        
        ga.sess$pagePathLevel4 <- substring(ga.sess$pagePathLevel4, 2)
        ga.sess %>%
            tidyr::separate(col = pagePathLevel4, into = c("state", "zip_code", "state_utility_slug"), sep = "/") %>%
            .[grep("-",.$state_utility_slug),] -> ga.sess.int
        
        ga.sess.int[nchar(ga.sess.int$zip_code) > 5, "state_utility_slug"] <- ga.sess.int[nchar(ga.sess.int$zip_code) > 5, "zip_code"]
        
        
        
        # Retrieve conversions to get conversion rate
        
        
        i <- 2
        incProgress(i/2, detail = paste("Doing part", i, "in", 2))
        ## Load token and validate ##
        token <- Auth(client.id = "XXXXXXXXX", client.secret = "XXXXXX")
        ValidateToken(token)
        
        ## Pull GA Leads Data by Channel & State##
        query.list <- Init(start.date = paste(start, sep = ""),
                           end.date = paste(end, sep = ""),
                           dimensions = "ga:date, ga:dimension1, ga:dimension2, ga:productCategoryHierarchy, ga:channelGrouping",
                           metrics = "ga:uniquePurchases",
                           max.results = 50000,
                           table.id = "ga:66293701",
                           filters = "ga:productCategoryHierarchy=~(electricity|naturalgas)")
        
        ga.query <- QueryBuilder(query.list)
        ga.trans <- GetReportData(ga.query, token, paginate_query = F)
        ga.trans <- filter(ga.trans, channelGrouping != 'Affiliate')
        ga.trans <- filter(ga.trans, channelGrouping != 'Call Center')
        
        ga.trans <- separate(ga.trans, col = productCategoryHierarchy, into = c("product", "service_type"), sep = "/")
        ga.trans <- aggregate(uniquePurchases~date+dimension1+dimension2+product, data=ga.trans, sum)
        
        
        ## Pull State Supplier List View to lookup correctly named state supplier slug for Supplier Name table ##
        
        if(launch.container == "dev"){
            con <- dbConnect(MySQL(),
                             user="mmichonski", password="R3dStapl3r",
                             dbname="nexus", host="prodreport.csvr4i6sfxp7.us-east-1.rds.amazonaws.com")
        }else{
            con <- dbConnect(MySQL(),
                             user="shinyapps", password="BiznatchAsUsualFiveOh999!",
                             dbname="nexus", host="prodreport.csvr4i6sfxp7.us-east-1.rds.amazonaws.com", port = 3306)
        }
        
        query <-"select utilities.slug, utilities.utility_id, utilities.utility_name, products.product_name
        from utilities
        
        left join products
        on utilities.product_id = products.product_id
        
        where dereg = b'1'"
        
        state_utilities_list <- dbGetQuery(con, query)
        dbDisconnect(con)
        
        
        })
        
        ## Find and replace incorrect state utility slugs with valid ones from Choose Energy databases ##
        
        #pb <- txtProgressBar(min = 0, max = length(state_utilities_list$slug), style = 3)
        withProgress(message = 'Part 4 of 6 - Gathering All Utility Zone Conversion Rates', value = 0,{
            for (i in 1:length(state_utilities_list$slug)){
                sul.i <- state_utilities_list$slug[i]
                if(sum(grepl(sul.i,ga.sess.int$state_utility_slug)) > 0){
                    ga.sess.int[grep(sul.i,ga.sess.int$state_utility_slug),"state_utility_slug"] <- sul.i
                } else{
                }
                #setTxtProgressBar(pb, i)
                incProgress(i/length(state_utilities_list$slug), detail = paste("Doing part", i, "in", length(state_utilities_list$slug)))
            }
            #close(pb)
        })
        
        
        ## Merge Dataframes Together and reformat ##
        
        merge(ga.sess.int, state_utilities_list, by.x = "state_utility_slug", by.y = "slug", all.x = TRUE) %>%
            filter(!is.na(utility_name)) %>%
            aggregate(uniquePageviews~date+state_utility_slug+utility_name+state+product_name+utility_id, data=., sum) %>%
            merge(ga.trans, by.x = c("utility_name", "state", "date", "product_name"), by.y = c("dimension2", "dimension1", "date", "product"), all.x = TRUE) %>%
            .[,!names(.) %in% c("service_type")] -> total
        
        total[is.na(total$uniquePurchases), "uniquePurchases"] <- 0
        total$date <- as.Date(parse_date_time(total$date, "Ymd"))
        
        total %<>%
            aggregate(cbind(uniquePageviews, uniquePurchases)~utility_name+date+product_name+state_utility_slug+utility_id, data=., sum) %>%
            mutate(cr = uniquePurchases/uniquePageviews)
        
        total[total$cr > 1, 'cr'] <-  1
        total[total$product_name == "electricity", 'product_name'] <-  "Electricity"
        total[total$product_name == "naturalgas", 'product_name'] <-  "Natural Gas"
        
        total %>% unite(col = utility_name_id, utility_name, utility_id, sep = " - ") %>%
            dplyr::rename(date_submitted = date) %>%
            dplyr::rename(product = product_name) %>%
            .[,!names(.) %in% c("state_utility_slug", "uniquePurchases", "utility_rank", "state")] -> sessions.ct
        
        total %>% unite(col = utility_name_id, utility_name, utility_id, sep = " - ") %>%
            dplyr::rename(date_submitted = date) %>%
            dplyr::rename(product = product_name) %>%
            .[,!names(.) %in% c("state_utility_slug", "uniquePageviews", "uniquePurchases", "utility_rank", "state")] -> cr.ct
        
    
    
        
        
        

        ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ##
        #       Causal Impact on Lead Volume for Days where Changes Ocurred      #
        ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ##
        
        
        ### Loook at the changes done on a given date to one individual utility zone and determine if significant change in lead volume occured ###
        
        
        changes.unique <- changes[!duplicated(changes[,c("date", "utility_id")]),]
        unique.index <- unique(changes.unique$change_index)
        
        greatest.impact <- c()
        #pb <- txtProgressBar(min = 0, max = length(unique.index), style = 3)
        withProgress(message = 'Part 5 of 6 - Calculating Causal Impact of Lead Volume Changes', value = 0,{
        for(i in 1:length(unique.index)){
            
            index <- unique.index[i]
            change.date.i <- max(changes[changes$change_index == index, "date"])
            if(change.date.i < today() - 2){
                product.i <- unique(changes[changes$change_index == index, "product"])
                utility.i <- paste(unique(changes[changes$change_index == index, "utility"]), unique(changes[changes$change_index == index, "utility_id"]), sep = " - ")
                leads.ct %>%
                    filter(utility_name_id == utility.i) %>%
                    filter(product == product.i) -> leads.i
                
                leads.ts <- zoo(leads.i[,!names(leads.i) %in% c("date_submitted", "utility_name_id", "product")], order.by = leads.i$date_submitted)
                pre.i <- c(min(leads.i$date_submitted), change.date.i)
                post.i <- c(change.date.i + 1, max(leads.i$date_submitted))
                
                if(max(leads.i$date_submitted) - change.date.i <= 1){
                    next
                } else{
                    
                    if(as.numeric(change.date.i - min(leads.i$date_submitted)) < 3){
                        next
                    } else{
                        
                        impact.i <- try.Causal.Impact(time_series = leads.ts, pre_date = pre.i, post_date = post.i, conf_int = 0.1)
                        
                        if(length(impact.i) == 1) {
                            next
                        } else {
                            
                            if(impact.i$summary$p[2] < 0.05){
                                
                                Average_Absolute_Effect <- impact.i$summary$AbsEffect[1]
                                Avg_Abs_Eff_Up_90_Conf_Interval <- impact.i$summary$AbsEffect.upper[1]
                                Avg_Abs_Eff_Lo_90_Conf_Interval <- impact.i$summary$AbsEffect.lower[1]
                                
                                Cumulative_Absolute_Effect <- impact.i$summary$AbsEffect[2]
                                Cum_Abs_Eff_Up_90_Conf_Interval <- impact.i$summary$AbsEffect.upper[2]
                                Cum_Abs_Eff_Lo_90_Conf_Interval <- impact.i$summary$AbsEffect.lower[2]
                                
                                Average_Relative_Effect <- impact.i$summary$RelEffect[1]
                                Avg_Rel_Eff_Up_90_Conf_Interval <- impact.i$summary$RelEffect.upper[1]
                                Avg_Rel_Eff_Lo_90_Conf_Interval <- impact.i$summary$RelEffect.lower[1]
                                
                                Cumulative_Relative_Effect <- impact.i$summary$RelEffect[2]
                                Cum_Rel_Eff_Up_90_Conf_Interval <- impact.i$summary$RelEffect.upper[2]
                                Cum_Rel_Eff_Lo_90_Conf_Interval <- impact.i$summary$RelEffect.lower[2]
                                
                                Prob_Causal_Effect <- (1 - impact.i$summary$p[2])
                                
                                change_index <- index
                                bayes.index <- i
                                
                                new.impact <- cbind(bayes.index,
                                                    change.date.i,
                                                    change_index,
                                                    product.i, 
                                                    utility.i,
                                                    Average_Absolute_Effect,
                                                    Avg_Abs_Eff_Up_90_Conf_Interval,
                                                    Avg_Abs_Eff_Lo_90_Conf_Interval,
                                                    Cumulative_Absolute_Effect,
                                                    Cum_Abs_Eff_Up_90_Conf_Interval,
                                                    Cum_Abs_Eff_Lo_90_Conf_Interval,
                                                    Average_Relative_Effect,
                                                    Avg_Rel_Eff_Up_90_Conf_Interval,
                                                    Avg_Rel_Eff_Lo_90_Conf_Interval,
                                                    Cumulative_Relative_Effect,
                                                    Cum_Rel_Eff_Up_90_Conf_Interval,
                                                    Cum_Rel_Eff_Lo_90_Conf_Interval,
                                                    Prob_Causal_Effect)
                                
                                greatest.impact <- rbind(greatest.impact, new.impact)
                                
                            } else{
                                next
                            }
                            
                        }
                        
                    }
                    
                }
                
                #setTxtProgressBar(pb, i)
                incProgress(i/length(unique.index), detail = paste("Doing part", i, "in", length(unique.index)))
            } else{
                next
            }
        }
        #close(pb)
        })
        
        greatest.impact %>%
            as.data.frame(stringsAsFactors = FALSE) -> puck
        
        puck$change.date.i <- as.Date(as.numeric(puck$change.date.i))
        puck$Average_Absolute_Effect <- signif(as.numeric(puck$Average_Absolute_Effect), digits = 2)
        puck$Avg_Abs_Eff_Up_90_Conf_Interval <- signif(as.numeric(puck$Avg_Abs_Eff_Up_90_Conf_Interval), digits = 2)
        puck$Avg_Abs_Eff_Lo_90_Conf_Interval <- signif(as.numeric(puck$Avg_Abs_Eff_Lo_90_Conf_Interval), digits = 2)
        puck$Cumulative_Absolute_Effect <- signif(as.numeric(puck$Cumulative_Absolute_Effect), digits = 2)
        puck$Cum_Abs_Eff_Up_90_Conf_Interval <- signif(as.numeric(puck$Cum_Abs_Eff_Up_90_Conf_Interval), digits = 2)
        puck$Cum_Abs_Eff_Lo_90_Conf_Interval <- signif(as.numeric(puck$Cum_Abs_Eff_Lo_90_Conf_Interval), digits = 2)
        puck$Average_Relative_Effect <- signif(as.numeric(puck$Average_Relative_Effect), digits = 2)
        puck$Avg_Rel_Eff_Up_90_Conf_Interval <- signif(as.numeric(puck$Avg_Rel_Eff_Up_90_Conf_Interval), digits = 2)
        puck$Avg_Rel_Eff_Lo_90_Conf_Interval <- signif(as.numeric(puck$Avg_Rel_Eff_Lo_90_Conf_Interval), digits = 2)
        puck$Cumulative_Relative_Effect <- signif(as.numeric(puck$Cumulative_Relative_Effect), digits = 2)
        puck$Cum_Rel_Eff_Up_90_Conf_Interval <- signif(as.numeric(puck$Cum_Rel_Eff_Up_90_Conf_Interval), digits = 2)
        puck$Cum_Rel_Eff_Lo_90_Conf_Interval <- signif(as.numeric(puck$Cum_Rel_Eff_Lo_90_Conf_Interval), digits = 2)
        puck$Prob_Causal_Effect <- percent(as.numeric(puck$Prob_Causal_Effect))
        
        puck %>%
            arrange(desc(abs(Average_Absolute_Effect))) -> final.impact
        
        
        
        ### Left Join all Plan Changes back onto biggest Causal Impact Events Table ###
        
        final.impact.full <- c()
        
        for(i in 1:dim(final.impact)[1]){
            change.index <- unique(final.impact$change_index)[i]
            
            change.i <- changes[changes$change_index == change.index, ]
            
            new.data <- data.frame(change.index,
                                   old_price = change.i[change.i$plan_audit_id == min(change.i$plan_audit_id), "plan_price"],
                                   new_price = change.i[change.i$plan_audit_id == max(change.i$plan_audit_id), "plan_price"],
                                   old_start_date = change.i[change.i$plan_audit_id == min(change.i$plan_audit_id), "start_date"],
                                   new_start_date = change.i[change.i$plan_audit_id == max(change.i$plan_audit_id), "start_date"],
                                   old_end_date = change.i[change.i$plan_audit_id == min(change.i$plan_audit_id), "end_date"],
                                   new_end_date = change.i[change.i$plan_audit_id == max(change.i$plan_audit_id), "end_date"],
                                   old_plan_status = change.i[change.i$plan_audit_id == min(change.i$plan_audit_id), "plan_status_name"],
                                   new_plan_status = change.i[change.i$plan_audit_id == max(change.i$plan_audit_id), "plan_status_name"]
            )
            
            final.impact.full <- rbind(final.impact.full, new.data)
            
        }
        
        final.impact.full <- dplyr::rename(final.impact.full, change_index = change.index)
        final.impact.full$change_index <- as.character(final.impact.full$change_index)
        final.impact.full$old_plan_status <- as.character(final.impact.full$old_plan_status)
        final.impact.full$new_plan_status <- as.character(final.impact.full$new_plan_status)
        
        changes %>%
            .[,!names(.) %in% c("product.i",
                                "utility.i",
                                "plan_utility_price_id",
                                "plan_utility_id",
                                "plan_audit_id",
                                "active",
                                "plan_utility_created",
                                "plan_utility_updated",
                                "plan_created",
                                "plans_updated",
                                "plan_price",
                                "realtime_end_date",
                                "date",
                                "original_active_date",
                                "plan_price",
                                "start_date",
                                "end_date",
                                "plan_status_name")] %>%
            .[!duplicated(.$change_index),] -> changes.deduped
        
        
        final.impact %>%
            left_join(final.impact.full, by ="change_index") %>%
            left_join(changes.deduped, by = "change_index") %>%
            .[,!names(.) %in% c("product.i", "utility.i")] %>%
            .[!duplicated(.), ] %>%
            mutate(plan_plus_utility_id = paste(plan_id, utility_id, sep = "-")) -> top.impacts.leads.all
        
        top.impacts.leads.all %>%
            .[!duplicated(.$utility_name_id),] -> top.impacts.leads
        

    
        
        
        
        
        
        ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ##
        #       Causal Impact on Conversion Rate for Days where Changes Ocurred      #
        ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ##
        
           
        
        
        ### Look at conversion rate changes on days where most significant changes occured in lead volume in one individual utility zone  ###
        
        changes.unique <- changes[!duplicated(changes[,c("date", "utility_id")]),]
        unique.index <- unique(changes.unique$change_index)
        
        greatest.impact <- c()
        #pb <- txtProgressBar(min = 0, max = length(unique.index), style = 3)
        withProgress(message = 'Part 6 of 6 - Calculating Causal Impact of Conversion Rate Changes', value = 0,{
            
            for(i in 1:dim(top.impacts.leads)[1]){
                
                index <- top.impacts.leads$change_index[i]
                change.date.i <- max(changes[changes$change_index == index, "date"])
                product.i <- unique(changes[changes$change_index == index, "product"])
                utility.i <- paste(unique(changes[changes$change_index == index, "utility"]), unique(changes[changes$change_index == index, "utility_id"]), sep = " - ")
                cr.ct %>%
                    filter(utility_name_id == utility.i) %>%
                    filter(product == product.i) -> leads.i
                
                leads.ts <- zoo(leads.i[,!names(leads.i) %in% c("date_submitted", "utility_name_id", "product")], order.by = leads.i$date_submitted)
                pre.i <- c(min(leads.i$date_submitted), change.date.i)
                post.i <- c(change.date.i + 1, max(leads.i$date_submitted))
                
                if(as.numeric(change.date.i - min(leads.i$date_submitted)) < 3){
                    next
                } else{
                    
                    impact.i <- try.Causal.Impact(time_series = leads.ts, pre_date = pre.i, post_date = post.i, conf_int = 0.1)
                    
                    if(length(impact.i) == 1) {
                        next
                    } else {
                        
                        
                        Average_Absolute_Effect <- impact.i$summary$AbsEffect[1]
                        Avg_Abs_Eff_Up_90_Conf_Interval <- impact.i$summary$AbsEffect.upper[1]
                        Avg_Abs_Eff_Lo_90_Conf_Interval <- impact.i$summary$AbsEffect.lower[1]
                        
                        Cumulative_Absolute_Effect <- impact.i$summary$AbsEffect[2]
                        Cum_Abs_Eff_Up_90_Conf_Interval <- impact.i$summary$AbsEffect.upper[2]
                        Cum_Abs_Eff_Lo_90_Conf_Interval <- impact.i$summary$AbsEffect.lower[2]
                        
                        Average_Relative_Effect <- impact.i$summary$RelEffect[1]
                        Avg_Rel_Eff_Up_90_Conf_Interval <- impact.i$summary$RelEffect.upper[1]
                        Avg_Rel_Eff_Lo_90_Conf_Interval <- impact.i$summary$RelEffect.lower[1]
                        
                        Cumulative_Relative_Effect <- impact.i$summary$RelEffect[2]
                        Cum_Rel_Eff_Up_90_Conf_Interval <- impact.i$summary$RelEffect.upper[2]
                        Cum_Rel_Eff_Lo_90_Conf_Interval <- impact.i$summary$RelEffect.lower[2]
                        
                        Prob_Causal_Effect <- (1 - impact.i$summary$p[2])
                        
                        change_index <- index
                        bayes.index <- as.numeric(top.impacts.leads$bayes.index[i])
                        
                        new.impact <- cbind(bayes.index,
                                            change.date.i,
                                            change_index,
                                            product.i, 
                                            utility.i,
                                            Average_Absolute_Effect,
                                            Avg_Abs_Eff_Up_90_Conf_Interval,
                                            Avg_Abs_Eff_Lo_90_Conf_Interval,
                                            Cumulative_Absolute_Effect,
                                            Cum_Abs_Eff_Up_90_Conf_Interval,
                                            Cum_Abs_Eff_Lo_90_Conf_Interval,
                                            Average_Relative_Effect,
                                            Avg_Rel_Eff_Up_90_Conf_Interval,
                                            Avg_Rel_Eff_Lo_90_Conf_Interval,
                                            Cumulative_Relative_Effect,
                                            Cum_Rel_Eff_Up_90_Conf_Interval,
                                            Cum_Rel_Eff_Lo_90_Conf_Interval,
                                            Prob_Causal_Effect)
                        
                        greatest.impact <- rbind(greatest.impact, new.impact)
                        
                    }
                    
                }
                
                #setTxtProgressBar(pb, i)
                incProgress(i/dim(top.impacts.leads)[1], detail = paste("Doing part", i, "in", dim(top.impacts.leads)[1]))
            }
            #close(pb)
        })
        
        greatest.impact %>%
            as.data.frame(stringsAsFactors = FALSE) -> puck
        
        puck$change.date.i <- as.Date(as.numeric(puck$change.date.i))
        puck$Average_Absolute_Effect <- signif(as.numeric(puck$Average_Absolute_Effect), digits = 2)
        puck$Avg_Abs_Eff_Up_90_Conf_Interval <- signif(as.numeric(puck$Avg_Abs_Eff_Up_90_Conf_Interval), digits = 2)
        puck$Avg_Abs_Eff_Lo_90_Conf_Interval <- signif(as.numeric(puck$Avg_Abs_Eff_Lo_90_Conf_Interval), digits = 2)
        puck$Cumulative_Absolute_Effect <- signif(as.numeric(puck$Cumulative_Absolute_Effect), digits = 2)
        puck$Cum_Abs_Eff_Up_90_Conf_Interval <- signif(as.numeric(puck$Cum_Abs_Eff_Up_90_Conf_Interval), digits = 2)
        puck$Cum_Abs_Eff_Lo_90_Conf_Interval <- signif(as.numeric(puck$Cum_Abs_Eff_Lo_90_Conf_Interval), digits = 2)
        puck$Average_Relative_Effect <- signif(as.numeric(puck$Average_Relative_Effect), digits = 2)
        puck$Avg_Rel_Eff_Up_90_Conf_Interval <- signif(as.numeric(puck$Avg_Rel_Eff_Up_90_Conf_Interval), digits = 2)
        puck$Avg_Rel_Eff_Lo_90_Conf_Interval <- signif(as.numeric(puck$Avg_Rel_Eff_Lo_90_Conf_Interval), digits = 2)
        puck$Cumulative_Relative_Effect <- signif(as.numeric(puck$Cumulative_Relative_Effect), digits = 2)
        puck$Cum_Rel_Eff_Up_90_Conf_Interval <- signif(as.numeric(puck$Cum_Rel_Eff_Up_90_Conf_Interval), digits = 2)
        puck$Cum_Rel_Eff_Lo_90_Conf_Interval <- signif(as.numeric(puck$Cum_Rel_Eff_Lo_90_Conf_Interval), digits = 2)
        puck$Prob_Causal_Effect <- percent(as.numeric(puck$Prob_Causal_Effect))
        
        final.impact <- puck
        
        top.impacts.cr <- merge(final.impact, top.impacts.leads[ , c("bayes.index", names(top.impacts.leads[ , !names(top.impacts.leads) %in% names(final.impact)]))], by = "bayes.index")
        
        top.impacts.cr <- top.impacts.cr[match(top.impacts.leads$bayes.index, top.impacts.cr$bayes.index), ]
        
        
        list(
            daily.prices, 
            changes, 
            leads.ct, 
            cr.ct, 
            top.impacts.leads, 
            top.impacts.cr,
            leads, 
            sessions.ct,
            plan.inv
        )

        

    })
    
    
    
    
    
    
    
            
            
            
        
            
            
            
            
            
            
        })