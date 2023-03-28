#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)


# turn off scientific notation
options(scipen=999)

# global params
METHOD = c("单独计税（奖金按3%税率计税，薪资按梯度计税）", "全部并入综合所得计税 (薪资加奖金按梯度计税)")
BASE_DEDUCT = 60000
TAX_RANGE = c(36000, 144000, 300000, 420000, 660000, 960000, Inf)
TAX_RATE = c(0.03, 0.1, 0.2, 0.25, 0.3, 0.35, 0.45)
BONUS_RATE = 0.03

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




# function to check input params
input_chk <- function(income, bonus, benefit, deduct) {
  null_vars <- c()
  if (is.null(cal(income))) null_vars <- append(null_vars, "年度薪资总收入")
  if (is.null(cal(bonus))) null_vars <- append(null_vars, "年度奖金总收入")
  if (is.null(cal(benefit))) null_vars <- append(null_vars, "年度三险一金总金额")
  if (is.null(cal(deduct))) null_vars <- append(null_vars, "专项抵扣金额")
  return(paste(null_vars, collapse = ", "))
}


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("税收计算器 - 打工人简易版"),
    prettyRadioButtons(
      inputId = "method",
      label = h3("计税方式"), 
      choices = METHOD,
      icon = icon("user"), 
      animation = "tada"
    ),
    textInput("income", label = h3("年度薪资总收入"), value = ""),
    textInput("bonus", label = h3("年度奖金总收入"), value = ""),
    textInput("benefit", label = h3("年度三险一金总金额 (个人部分)"), value = ""),
    textInput("deduct", label = h3("专项附加扣除金额"), value = ""),
    actionBttn(
      inputId = "submit",
      label = "计算",
      style = "simple", 
      color = "primary"
    ),
    actionBttn(
      inputId = "clear",
      label = "重置",
      style = "simple", 
      color = "danger"
    ),
    hr(),
    h5("注 1：本工具计税方法依据年度个人所得税累进税率，与官方的个人所得税APP采取同等方法。"),
    h5("注 2：金额内只可填写数字或仅含数字和数学符号的式子，例如：60000 或 5000 * 12 或 5000 + 5000 + 5000，以此类推。"),
    h5("注 3：若无此金额，请填写为0。"),
    tags$a(href="https://www.fadada.com/notice/detail-16666.html", h5("注 4：梯度扣税及专项附加扣除金额解释。")),
    tags$a(href="https://github.com/Cosmopolitan-Ou/TaxCal/blob/main/app.R", h5("注 5：源代码。")),
    hr(),
    h3(textOutput("rel")),
    h4(textOutput("value1")),
    h4(textOutput("value2")),
    h4(textOutput("value3")),
    h4(textOutput("value4")),
    h4(textOutput("value5")),
    hr()
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # submit button
  observeEvent(input$submit, {
    null_vars <- input_chk(input$income, input$bonus, input$benefit, input$deduct)
    if (null_vars != "") {
      msg <- paste0(null_vars, "的输入不正确，应该为数字或数学式子，如无金额请输入0！")
      showNotification(msg, type = "error")
    } else {
      vals <- cal2(input$income, input$bonus, input$benefit, input$deduct, input$method)
      if (is.null(vals)) {
        showNotification("税前总收入低于抵扣额+免税额，请检查输入金额！", type = "error")
      } else {
        output$rel <- renderText({"计税结果："})
        output$value1 <- renderText({paste0("税前总收入：￥", vals[1])})
        output$value2 <- renderText({paste0("总抵扣额：￥", vals[2], " (含免税额￥", BASE_DEDUCT, ")")})
        output$value3 <- renderText({paste0("应纳税所得额（纳税基数）：￥", vals[3])})
        output$value4 <- renderText({paste0("应纳税额：￥", vals[4], ifelse(vals[6] == METHOD[1], paste0(" (含奖金扣税", BONUS_RATE * 100, "%)"), ""))})
        output$value5 <- renderText({paste0("税后总收入（扣除三险一金）：￥", vals[5])})
      }
    }
    
  })
  
  # clear button
  observeEvent(input$clear, {
    updateTextInput(session, "income", value = "")
    updateTextInput(session, "bonus", value = "")
    updateTextInput(session, "benefit", value = "")
    updateTextInput(session, "deduct", value = "")
    output$rel <- renderText({""})
    output$value1 <- renderText({""})
    output$value2 <- renderText({""})
    output$value3 <- renderText({""})
    output$value4 <- renderText({""})
    output$value5 <- renderText({""})
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
