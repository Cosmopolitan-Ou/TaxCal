
# UI for application
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
  tags$a(href="https://github.com/Cosmopolitan-Ou/TaxCal", h5("注 5：源代码。")),
  hr(),
  h3(textOutput("rel")),
  h4(textOutput("value1")),
  h4(textOutput("value2")),
  h4(textOutput("value3")),
  h4(textOutput("value4")),
  h4(textOutput("value5")),
  hr()
)
