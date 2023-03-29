
# server logic
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
