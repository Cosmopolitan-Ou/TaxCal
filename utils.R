
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
    tax <- 0
  } else {
    resid <- c()
    for (i in TAX_RANGE) {
      resid <- append(resid, pre_amout - i)
    }
    tax <- 0
    for (i in seq_along(resid)) {
      tax <- tax + ifelse(resid[i] > 0, (TAX_RANGE[i] -  ifelse(i==1, 0, TAX_RANGE[i-1])) * (TAX_RATE[i]), 0)
      if (resid[i] < 0) break
    }
    tax <- tax + (pre_amout - ifelse(i == 1, 0, TAX_RANGE[i - 1])) * TAX_RATE[i]
  }
  
  tax <- tax + bonus_tax
  #############################################################################
  
  val3 <- ifelse(pre_amout < 0, 0, pre_amout)
  val4 <- tax
  val5 <- val1 - benefit - tax
  val6 <- method
  
  return(list(val1, val2, val3, val4, val5, val6))
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


