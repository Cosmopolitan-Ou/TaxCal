library(shiny)
library(shinyWidgets)
library(ggplot2)
library(showtext)


# turn off scientific notation
options(scipen=999)


# global parameters
METHOD = c("单独计税（奖金按3%税率计税，薪资按梯度计税）", "全部并入综合所得计税 (薪资加奖金按梯度计税)")
BASE_DEDUCT = 60000
TAX_RANGE = c(36000, 144000, 300000, 420000, 660000, 960000, Inf)
TAX_RATE = c(0.03, 0.1, 0.2, 0.25, 0.3, 0.35, 0.45)
BONUS_RATE = 0.03


############################# until functions ##################################

# function to evaluate math expression
cal <- function(txt) {
  if (grepl("[A-Za-z]", txt)) return(NULL)
  out <- tryCatch(
    {eval(parse(text=txt))},
    error=function(cond) {
      return(NULL)
    }
  )
  return(out)
}


# function to calculate results
cal2 <- function(income, bonus, benefit, deduct, method) {
  
  income <- cal(income)
  bonus <- cal(bonus)
  benefit <- cal(benefit)
  deduct <- cal(deduct)
  
  val1 <- income + bonus
  val2 <- BASE_DEDUCT + benefit + deduct
  
  
  # calculate tax
  #############################################################################
  if (method == METHOD[1]) {
    pre_amout <- income - val2
    bonus_tax <- bonus * BONUS_RATE
  } else if (method == METHOD[2]) {
    pre_amout <- val1 - val2
    bonus_tax <- 0
  }
  
  if (pre_amout <= 0) {
    tax <- c(0)
  } else {
    resid <- c()
    for (i in TAX_RANGE) {
      resid <- append(resid, pre_amout - i)
    }
    tax <- c()
    for (i in seq_along(resid)) {
      tax <- append(tax, ifelse(resid[i] > 0, (TAX_RANGE[i] -  ifelse(i==1, 0, TAX_RANGE[i-1])) * (TAX_RATE[i]), 0))
      if (resid[i] < 0) break
    }
    tax <- append(tax[1:length(tax)-1], (pre_amout - ifelse(i == 1, 0, TAX_RANGE[i - 1])) * TAX_RATE[i])
  }
  
  tax_total <- sum(tax) + bonus_tax
  #############################################################################
  
  # get summary numbers
  val3 <- ifelse(pre_amout < 0, 0, pre_amout)
  val4 <- tax_total
  val5 <- val1 - benefit - tax_total
  val6 <- method
  
  # get tax details
  tax_class <- append(rep(paste("累计基数：￥", val3), length(tax)), paste0("奖金基数：￥", bonus))
  tax_range <- c()
  for (i in seq_along(TAX_RANGE)) {
    if (i == 1) {
      tax_range <- append(tax_range, paste0("0 ~ ", TAX_RANGE[i] / 10000, " 万 (", TAX_RATE[i] * 100, "%)"))
    } else if(i != length(TAX_RANGE)){
      tax_range <- append(tax_range, paste0(TAX_RANGE[i-1] / 10000, " ~ ", TAX_RANGE[i] / 10000, " 万 (", TAX_RATE[i] * 100, "%)"))
    } else {
      tax_range <- append(tax_range, paste0("> ", TAX_RANGE[i - 1] / 10000, " 万 (", TAX_RATE[i] * 100, "%)"))
    }
  }
  tax_range <- append(tax_range[1:length(tax)], paste0("奖金部分 (", BONUS_RATE * 100, "%)"))
  tax_txt <- c()
  for(i in tax) {
    tax_txt <- append(tax_txt, paste0("￥", i))
  }
  tax_txt <- append(tax_txt, paste0("￥", bonus_tax))
  # tax_cumsum_bonus <- append(tax_cumsum, bonus_tax)
  tax_ladder_bonus <- append(tax, bonus_tax)
  df_tax <- data.frame(tax_class, tax_ladder_bonus, tax_txt)
  df_tax[["tax_range"]] <- factor(tax_range, levels = tax_range)
  
  tax_sum <- c(paste0("奖金税额：￥", bonus_tax), paste0("梯度税额：￥", sum(tax)))
  
  return(list(val1, val2, val3, val4, val5, val6, df_tax, tax_sum))
}

# function to plot tax details
tax_plt <- function(df_tax, tax_sum) {
  showtext_auto()
  plt <- ggplot(df_tax, aes(fill=tax_range, y=tax_ladder_bonus, x=tax_class, label=tax_txt)) + 
    geom_bar(position="stack", stat="identity") +
    geom_text(size = 3, position = position_stack(vjust = 0.5)) +
    theme(axis.title.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.title.y=element_text(angle = 0, vjust = 0.5),
          panel.grid.major = element_blank(),
          legend.position = "top") +
    ylab("扣税额") +
    annotate("text",x=1:2,y=Inf,vjust=1.5,label=tax_sum) +
    labs(fill="扣税占比：")
  return(plt)
}


# function to check input parameters
input_chk <- function(income, bonus, benefit, deduct) {
  null_vars <- c()
  if (is.null(cal(income))) null_vars <- append(null_vars, "年度薪资总收入")
  if (is.null(cal(bonus))) null_vars <- append(null_vars, "年度奖金总收入")
  if (is.null(cal(benefit))) null_vars <- append(null_vars, "年度三险一金总金额 (个人部分)")
  if (is.null(cal(deduct))) null_vars <- append(null_vars, "专项附加扣除金额")
  return(paste(null_vars, collapse = ", "))
}
