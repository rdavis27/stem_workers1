library(shiny)
library(ggplot2)
library("foreign")
#library(AER)  # contains ivreg (old)
library(ivreg) # contains ivreg
library(plotly)

# Replace 'ui <- fluidPage('...')' with
#       'shinyUI(fluidPage('...'))' if in separate ui.R file
ui <- fluidPage(
    titlePanel("The Effects of Foreign STEM on Native Wages and Employment"),
    sidebarLayout(
        sidebarPanel(
            width = 2,
            selectInput("tspans", "Time spans",
                        choices = c("1990-00-05-10","1990-00-10"),
                        selected = "1990-00-05-10",
                        multiple = FALSE),
            selectInput("grate", "Growth rate",
                        choices = c("Timespan growth","1-year growth","5-year growth"),
                        selected = "Timespan growth",
                        multiple = FALSE),
            checkboxInput("usemets","Use metareas",value = TRUE),
            checkboxInput("uselogs","Use logs",value = FALSE)
        ),
        mainPanel(
            width = 10,
            tabsetPanel(
                type = "tabs",
                tabPanel("Plotly",
                    sidebarPanel(
                        width = 2,
                        selectInput("yvar", "Y variable",
                                    choices = c("pDiff","Pvalue","Slope"),
                                    selected = "pDiff",
                                    multiple = FALSE),
                        textInput("vcolor","Color",value = "black,blue2,orange2,red2"),
                        textInput("vshape","Shape",value = "1,10,16,17"),
                        checkboxInput("mark0","Mark x-axis",value = TRUE)
                    ),
                    mainPanel(
                        width = 10,
                        plotlyOutput(outputId = "myPlotly")
                    )
                ),
                tabPanel("Text", verbatimTextOutput("myText")),
                tabPanel("Usage", htmlOutput(outputId = "myUsage")
                )
            )
        )
    )
)

# Replace 'server <- function(input, output) {'...'}' with
#       'shinyServer(function(input, output) {'...'})' if in separate server.R file
server <- function(input, output) {
    output$myUsage <- renderUI({
        includeHTML("http://econdataus.com/stem_workers1.htm")
    })
    output$myPlotly <- renderPlotly({
        dd <- myData()
        labelLevels <- c("Wage_STEM","Wage_College","Wage_Non_College",
                         "Empl_STEM","Empl_College","Empl_Non_College")
        dd$Label <- factor(labelLevels, labelLevels)
        dd$Sig <- 0
        dd$Sig[dd$Pvalue < 0.1] <- 1
        dd$Sig[dd$Pvalue < 0.05] <- 2
        dd$Sig[dd$Pvalue < 0.01] <- 3
        dd$Significance <- as.factor(dd$Sig)
        gg <- ggplot(data=dd, aes_string(x="Label",y=input$yvar))
        gg <- gg + geom_point(data=dd ,aes_string(color="Significance",shape="Significance"), size=5, alpha=1.0)
        vcolor <- unlist(strsplit(input$vcolor, ","))
        if (length(vcolor) > 1){
            gg <- gg + scale_fill_manual(values = vcolor) # Bar Plot
            gg <- gg + scale_color_manual(values = vcolor) # Line Graph
        }
        vshape <- as.numeric(unlist(strsplit(input$vshape, ",")))
        if (length(vshape) > 1){
            gg <- gg + scale_shape_manual(values = vshape)
        }
        if (input$mark0){
            gg <- gg + geom_hline(yintercept=0, color="black")
        }
        gg <- gg + ggtitle(paste0("<b>The Effects of Foreign STEM on Native Wages and Employment</b><br>",
                           get_subtitle()))
        gg <- gg + xlab("Factor")
        if (input$yvar == "pDiff"){
            gg <- gg + ylab("% difference of Slope from Study")
        }
        else{
            gg <- gg + ylab(input$yvar)
        }
        gg
    })
    output$myText <- renderPrint({
        dd <- myData()
        title <- "Table 5 - The Effects of Foreign STEM on Native Wages and Employment\n"
        subtitle <- get_subtitle()
        cat("\n")
        cat(title)
        cat("\n")
        cat(subtitle)
        cat("\n")
        ivreg_print(dd)
        return("")
    })
    get_subtitle <- function(){
        subtitle <- ""
        if (input$uselogs) subtitle <- "Log of "
        subtitle <- paste0(subtitle,input$grate,", ")
        if (input$usemets) subtitle <- paste0(subtitle,"with metarea, ")
        else subtitle <- paste0(subtitle,"without metarea, ")
        subtitle <- paste0(subtitle,input$tspans,"\n")
        return(subtitle)
    }
    ivreg_print <- function(xx){
        cat(" N  INTERCEPT     SLOPE    STUDY   % DIFF     S.E.   T-STAT  P-VALUE  DESCRIPTION\n")
        cat("--  ---------  --------  -------  -------  -------  -------  -------  -----------------------------------\n")
        for (i in 1:NROW(xx)){
            vv <- xx[i,]
            cat(sprintf("%2d  %9.4f %9.4f %8.4f %8.2f %8.3f %8.3f %8.3f  %s\n",
                        i, vv$Intercept, vv$Slope, vv$Study, vv$pDiff,
                        vv$SE, vv$Tstat, vv$Pvalue, vv$Description))
        }
    }
    save_mm <- function(vi, ivr, coef1, coef2, sum, study){
        pdiff <- 100* (coef2 - study) / study
        sum22 <- sum[2,2]
        sum23 <- sum[2,3]
        sum24 <- sum[2,4]
        newrow <- data.frame(vi, coef1, coef2, study, pdiff, sum22, sum23, sum24)
        ivr <- rbind(ivr, newrow)
    }
    ivreg_all_save <- function(ff, ivr){
        zff <<- ff #DEBUG-RM
        met <- "OLS" # hardcode
        mm <- ivreg(delta_native_stemO4_wkwage ~ delta_imm_stemO4 +fspan + fmets + bartik_coll_wage   + bartik_coll_emp   | . + delta_imm_stemO4_H1B_hat80 - delta_imm_stemO4, data=ff, method = met)
        ivr <- save_mm(1, ivr, mm$coefficients[1], mm$coefficients[2], summary(mm)$coefficients, 6.65)
        mm <- ivreg(delta_native_coll_wkwage   ~ delta_imm_stemO4 +fspan + fmets + bartik_coll_wage   + bartik_coll_emp   | . + delta_imm_stemO4_H1B_hat80 - delta_imm_stemO4, data=ff, method = met)
        ivr <- save_mm(2, ivr, mm$coefficients[1], mm$coefficients[2], summary(mm)$coefficients, 8.03)
        mm <- ivreg(delta_native_nocoll_wkwage ~ delta_imm_stemO4 +fspan + fmets + bartik_nocoll_wage + bartik_nocoll_emp | . + delta_imm_stemO4_H1B_hat80 - delta_imm_stemO4, data=ff, method = met)
        ivr <- save_mm(3, ivr, mm$coefficients[1], mm$coefficients[2], summary(mm)$coefficients, 3.78)
        mm <- ivreg(delta_native_stemO4_emp    ~ delta_imm_stemO4 +fspan + fmets + bartik_coll_wage   + bartik_coll_emp   | . + delta_imm_stemO4_H1B_hat80 - delta_imm_stemO4, data=ff, method = met)
        ivr <- save_mm(4, ivr, mm$coefficients[1], mm$coefficients[2], summary(mm)$coefficients, 0.53)
        mm <- ivreg(delta_native_coll_emp      ~ delta_imm_stemO4 +fspan + fmets + bartik_coll_wage   + bartik_coll_emp   | . + delta_imm_stemO4_H1B_hat80 - delta_imm_stemO4, data=ff, method = met)
        ivr <- save_mm(5, ivr, mm$coefficients[1], mm$coefficients[2], summary(mm)$coefficients, 2.48)
        mm <- ivreg(delta_native_nocoll_emp    ~ delta_imm_stemO4 +fspan + fmets + bartik_nocoll_wage + bartik_nocoll_emp | . + delta_imm_stemO4_H1B_hat80 - delta_imm_stemO4, data=ff, method = met)
        ivr <- save_mm(6, ivr, mm$coefficients[1], mm$coefficients[2], summary(mm)$coefficients,-5.17)
        return(ivr)
    }    
    ivreg_nomets_save <- function(ff, ivr){
        met <- "OLS" # hardcode
        mm <- ivreg(delta_native_stemO4_wkwage ~ delta_imm_stemO4 +fspan + bartik_coll_wage   + bartik_coll_emp   | . + delta_imm_stemO4_H1B_hat80 - delta_imm_stemO4, data=ff, method = met)
        ivr <- save_mm(1, ivr, mm$coefficients[1], mm$coefficients[2], summary(mm)$coefficients, 6.65)
        mm <- ivreg(delta_native_coll_wkwage   ~ delta_imm_stemO4 +fspan + bartik_coll_wage   + bartik_coll_emp   | . + delta_imm_stemO4_H1B_hat80 - delta_imm_stemO4, data=ff, method = met)
        ivr <- save_mm(2, ivr, mm$coefficients[1], mm$coefficients[2], summary(mm)$coefficients, 8.03)
        mm <- ivreg(delta_native_nocoll_wkwage ~ delta_imm_stemO4 +fspan + bartik_nocoll_wage + bartik_nocoll_emp | . + delta_imm_stemO4_H1B_hat80 - delta_imm_stemO4, data=ff, method = met)
        ivr <- save_mm(3, ivr, mm$coefficients[1], mm$coefficients[2], summary(mm)$coefficients, 3.78)
        mm <- ivreg(delta_native_stemO4_emp    ~ delta_imm_stemO4 +fspan + bartik_coll_wage   + bartik_coll_emp   | . + delta_imm_stemO4_H1B_hat80 - delta_imm_stemO4, data=ff, method = met)
        ivr <- save_mm(4, ivr, mm$coefficients[1], mm$coefficients[2], summary(mm)$coefficients, 0.53)
        mm <- ivreg(delta_native_coll_emp      ~ delta_imm_stemO4 +fspan + bartik_coll_wage   + bartik_coll_emp   | . + delta_imm_stemO4_H1B_hat80 - delta_imm_stemO4, data=ff, method = met)
        ivr <- save_mm(5, ivr, mm$coefficients[1], mm$coefficients[2], summary(mm)$coefficients, 2.48)
        mm <- ivreg(delta_native_nocoll_emp    ~ delta_imm_stemO4 +fspan + bartik_nocoll_wage + bartik_nocoll_emp | . + delta_imm_stemO4_H1B_hat80 - delta_imm_stemO4, data=ff, method = met)
        ivr <- save_mm(6, ivr, mm$coefficients[1], mm$coefficients[2], summary(mm)$coefficients,-5.17)
    }    
    adjust_xy <- function(df){
        df$delta_native_stemO4_wkwage <- (1 + df$diff_native_stemO4_wkwage / df$nat_stemO4_wkwage) ^ (1 / df$nyrs) - 1
        df$delta_native_coll_wkwage   <- (1 + df$diff_native_coll_wkwage   / df$nat_coll_wkwage) ^ (1 / df$nyrs) - 1
        df$delta_native_nocoll_wkwage <- (1 + df$diff_native_nocoll_wkwage / df$nat_nocoll_wkwage) ^ (1 / df$nyrs) - 1
        df$delta_native_stemO4_emp    <- (1 + df$diff_native_stemO4_emp    / df$labforce) ^ (1 / df$nyrs) - 1
        df$delta_native_coll_emp      <- (1 + df$diff_native_coll_emp      / df$labforce) ^ (1 / df$nyrs) - 1
        df$delta_native_nocoll_emp    <- (1 + df$diff_native_nocoll_emp    / df$labforce) ^ (1 / df$nyrs) - 1
        df$delta_imm_stemO4           <- (1 + df$diff_imm_stemO4           / df$labforce) ^ (1 / df$nyrs) - 1
        df$delta_imm_stemO4_H1B_hat80 <- (1 + df$diff_imm_stemO4_H1B_hat80 / df$labforce_hat80) ^ (1 / df$nyrs) - 1
        df
    }
    adjust_log_xy <- function(df){
        df$delta_native_stemO4_wkwage <- log((1 + df$diff_native_stemO4_wkwage / df$nat_stemO4_wkwage) ^ (1 / df$nyrs))
        df$delta_native_coll_wkwage   <- log((1 + df$diff_native_coll_wkwage   / df$nat_coll_wkwage) ^ (1 / df$nyrs))
        df$delta_native_nocoll_wkwage <- log((1 + df$diff_native_nocoll_wkwage / df$nat_nocoll_wkwage) ^ (1 / df$nyrs))
        df$delta_native_stemO4_emp    <- log((1 + df$diff_native_stemO4_emp    / df$labforce) ^ (1 / df$nyrs))
        df$delta_native_coll_emp      <- log((1 + df$diff_native_coll_emp      / df$labforce) ^ (1 / df$nyrs))
        df$delta_native_nocoll_emp    <- log((1 + df$diff_native_nocoll_emp    / df$labforce) ^ (1 / df$nyrs))
        df$delta_imm_stemO4           <- log((1 + df$diff_imm_stemO4           / df$labforce) ^ (1 / df$nyrs))
        df$delta_imm_stemO4_H1B_hat80 <- log((1 + df$diff_imm_stemO4_H1B_hat80 / df$labforce_hat80) ^ (1 / df$nyrs))
        df
    }
    adjust_bartik <- function(df){
        df$bartik_coll_wage      <- (1 + df$diff_bartik_coll_wage   / df$pred_coll_wkwage) ^ (1 / df$nyrs) - 1
        df$bartik_nocoll_wage    <- (1 + df$diff_bartik_nocoll_wage / df$pred_nocoll_wkwage) ^ (1 / df$nyrs) - 1
        df$bartik_coll_emp       <- (1 + df$diff_bartik_coll_emp    / df$pred_emp) ^ (1 / df$nyrs) - 1
        df$bartik_nocoll_emp     <- (1 + df$diff_bartik_nocoll_emp  / df$pred_emp) ^ (1 / df$nyrs) - 1
        df
    }
    adjust_log_bartik <- function(df){
        df$bartik_coll_wage      <- log((1 + df$diff_bartik_coll_wage   / df$pred_coll_wkwage) ^ (1 / df$nyrs))
        df$bartik_nocoll_wage    <- log((1 + df$diff_bartik_nocoll_wage / df$pred_nocoll_wkwage) ^ (1 / df$nyrs))
        df$bartik_coll_emp       <- log((1 + df$diff_bartik_coll_emp    / df$pred_emp) ^ (1 / df$nyrs))
        df$bartik_nocoll_emp     <- log((1 + df$diff_bartik_nocoll_emp  / df$pred_emp) ^ (1 / df$nyrs))
        df
    }
    myData <- reactive({
        dd <- myFile()
        if (input$tspans == "1990-00-05-10"){
            ff <- dd[dd$span >= 2 & dd$span <= 4,]
            labyears <- "1990-2010"
        }
        else if (input$tspans == "1990-00-10"){
            ff <- dd[dd$span >= 2 & dd$span <= 3,]
            labyears <- "1990-2010"
        }
        else{
            cat(file = stderr(), paste0("ERROR: Unknown Time spans: ",input$tspans,"\n"))
        }
        if (input$grate == "1-year growth"){
            ff$nyrs <- 5 # 5 years
            ff$nyrs[ff$year==1980] <- 10 # 10 years
            ff$nyrs[ff$year==1990] <- 10 # 10 years
            if (input$tspans == "1990-00-10"){
                ff$nyrs[ff$year==2000] <- 10 # 10 years
            }
        }
        else if (input$grate == "5-year growth"){
            ff$nyrs <- 1 # 5 years
            ff$nyrs[ff$year==1980] <- 2 # 10 years
            ff$nyrs[ff$year==1990] <- 2 # 10 years
            if (input$tspans == "1990-00-10"){
                ff$nyrs[ff$year==2000] <- 2 # 10 years
            }
        }
        else if (input$grate == "Timespan growth"){
            ff$nyrs <- 1 # all timespans
        }
        else{
            cat(file = stderr(), paste0("ERROR: Unknown Growth rate: ",input$grate,"\n"))
        }
        for(ii in 1:ncol(ff)) {
            jj=sum(is.na(ff[,ii]))
            if (jj > 0) {
                cat(paste0(colnames(ff)[ii], " contains ", jj, " NAs\n"))
            }
        }
        ff <- na.omit(ff)

        if (input$uselogs){
            ff <- adjust_log_xy(ff)
            ff <- adjust_log_bartik(ff)
        }
        else{
            ff <- adjust_xy(ff)
            ff <- adjust_bartik(ff)
        }
        vi <- numeric(0)
        coef1 <- numeric(0)
        coef2 <- numeric(0)
        study <- numeric(0)
        pdiff <- numeric(0)
        sum22 <- numeric(0)
        sum23 <- numeric(0)
        sum24 <- numeric(0)
        ivr <- data.frame(vi, coef1, coef2, study, pdiff, sum22, sum23, sum24)
        if (input$usemets){
            ivr <- ivreg_all_save(ff, ivr)
        }
        else{
            ivr <- ivreg_nomets_save(ff, ivr)
        }
        xx <- ivr[,c(-1)]
        rownames(xx) <- seq(1,6)
        colnames(xx) <- c("Intercept","Slope","Study","pDiff","SE","Tstat","Pvalue")
        xx$Description <- c("Weekly Wage, Native STEM",
                            "Weekly Wage, Native College-Educated",
                            "Weekly Wage, Native Non-College-Educated",
                            "Employment, Native STEM",
                            "Employment, Native College-Educated",
                            "Employment, Native Non-College-Educated")
        #cols_align(dd, align = "left", columns = "Description")
        zxx <<- xx
        return(xx)
    })
    myFile <- reactive({
        #cc <- read.dta("13042data\\JOLE Data & Do Files\\JOLE_regressions_tables\\PSS_Data.dta")
        cc <- read.dta("PSS_Data.dta")
        zcc <<- cc #DEBUG-RM
        #print(summary(cc$year))
        
        cc$span <- 0
        cc$span[cc$year==1980] <- 1
        cc$span[cc$year==1990] <- 2
        cc$span[cc$year==2000] <- 3
        if (input$tspans == "1990-00-10"){
            cc <- cc[cc$year != 2005,] # remove 2005
            cc$span[cc$year==2010] <- 4
            cc$nyrs <- 1 # timespan
        }
        else{
            cc$span[cc$year==2005] <- 4
            cc$span[cc$year==2010] <- 5
            cc$nyrs <- 1 # timespan
        }
        
        dd <- cc[order(cc$metarea, cc$year),]
        dd <- dd[dd$panel1980 == 1,] # 219 common metareas
        dd$diff_native_stemO4_wkwage  <- c(diff(dd$nat_stemO4_wkwage),    0)
        dd$diff_native_coll_wkwage    <- c(diff(dd$nat_coll_wkwage),      0)
        dd$diff_native_nocoll_wkwage  <- c(diff(dd$nat_nocoll_wkwage),    0)
        dd$diff_native_stemO4_emp     <- c(diff(dd$nat_stemO4_emp),       0)
        dd$diff_native_coll_emp       <- c(diff(dd$nat_coll_emp),         0)
        dd$diff_native_nocoll_emp     <- c(diff(dd$nat_nocoll_emp),       0)
        dd$diff_imm_stemO4            <- c(diff(dd$imm_stemO4),           0)
        dd$diff_imm_stemO4_H1B_hat80  <- c(diff(dd$imm_stemO4_H1B_hat80), 0)
        
        dd$delta_native_stemO4_wkwage <- dd$diff_native_stemO4_wkwage / dd$nat_stemO4_wkwage
        dd$delta_native_coll_wkwage   <- dd$diff_native_coll_wkwage   / dd$nat_coll_wkwage
        dd$delta_native_nocoll_wkwage <- dd$diff_native_nocoll_wkwage / dd$nat_nocoll_wkwage
        dd$delta_native_stemO4_emp    <- dd$diff_native_stemO4_emp    / dd$labforce
        dd$delta_native_coll_emp      <- dd$diff_native_coll_emp      / dd$labforce
        dd$delta_native_nocoll_emp    <- dd$diff_native_nocoll_emp    / dd$labforce
        dd$delta_imm_stemO4           <- dd$diff_imm_stemO4           / dd$labforce
        dd$delta_imm_stemO4_H1B_hat80 <- dd$diff_imm_stemO4_H1B_hat80 / dd$labforce_hat80
        
        dd$diff_bartik_coll_wage      <- c(diff(dd$pred_coll_wkwage),   0)
        dd$diff_bartik_nocoll_wage    <- c(diff(dd$pred_nocoll_wkwage), 0)
        dd$diff_bartik_coll_emp       <- c(diff(dd$pred_coll_emp),      0)
        dd$diff_bartik_nocoll_emp     <- c(diff(dd$pred_nocoll_emp),    0)
        
        dd$bartik_coll_wage      <- dd$diff_bartik_coll_wage   / dd$pred_coll_wkwage
        dd$bartik_nocoll_wage    <- dd$diff_bartik_nocoll_wage / dd$pred_nocoll_wkwage
        dd$bartik_coll_emp       <- dd$diff_bartik_coll_emp    / dd$pred_emp
        dd$bartik_nocoll_emp     <- dd$diff_bartik_nocoll_emp  / dd$pred_emp
        
        dd$fspan <- as.factor(dd$span)
        dd$fmets <- as.factor(dd$metarea)
        zdd <<- dd #DEBUG-RM
        return(dd)
    })
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)