# https://henryquant.shinyapps.io/penguin/
# - palmerpenguins 패키지의 penguins 데이터 이용
# - 사이드바에는 총 4개 위젯
# 1. 펭귄 종류 (checkboxGroupInput)
# 2. 그림의 x축 (selectInput)
# 3. 그림의 y축 (selectInput)
# 4. 그림의 점 크기 (sliderInput)
# - 메인에는 총 2개 데이터
# 1. 전체 데이터 중 위젯에서 선택한 펭귄 데이터만 datatable로 표현
# 2. 선택한 펭귄 데이터 중 위젯에서 고른 x축과 y축 기반으로 그림
# 1. 성별(sex)은 shape 다르게 표현
# 2. 위젯에서 선택한 데이터에 따라 점 크기 변경
# - **shinyapps 혹은 온라인에 업로드 후 URL만 보내주세요. 코드 X**

library(shiny)
library(palmerpenguins)
library(DT)
library(ggplot2)

# UI 구성 요소
ui <- fluidPage(
  titlePanel("펭귄 데이터 분석"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(
        inputId = "species",
        label = "펭귄 종류를 선택하세요",
        choices = unique(penguins$species),
        selected = unique(penguins$species)
      ),
      selectInput(
        inputId = "x_axis",
        label = "x축을 선택하세요.",
        choices = c("bill_length_mm", "bill_depth_mm", "filpper_length_mm", "body_mass_g"),
        selected = "bill_length_mm"
      ),
      selectInput(
        inputId = "y_axis",
        label = "y축을 선택하세요.",
        choices = c("bill_length_mm", "bill_depth_mm", "filpper_length_mm", "body_mass_g"),
        selected = "body_mass_g"
      ),
      sliderInput(
        inputId = "point_size",
        label = "점 크기를 선택하세요",
        min = 1,
        max = 10,
        value = 5
      )
    ),
    mainPanel(
      DTOutput("filtered_data"),
      plotOutput("scatter_plot")
    )
  )
)

# 서버 로직
server <- function(input, output) {
  
  # 필터링된 데이터 생성
  filtered_data <- reactive({
    penguins_filtered <- subset(penguins, species %in% input$species)
    return(penguins_filtered)
  })
  
  # 결과값 표시 (테이블)
  output$filtered_data <- renderDT({
    datatable(filtered_data())
  })
  
  # 산점도 그리기
  output$scatter_plot <- renderPlot({
    penguins_filtered <- filtered_data()
    p <- ggplot(penguins_filtered, aes_string(x = input$x_axis, y = input$y_axis)) +
      geom_point(aes(color = species, shape = sex), size = input$point_size) +
      labs(x = input$x_axis, y = input$y_axis)
    print(p)
  })
  
}

# Shiny 앱 실행
shinyApp(ui = ui, server = server)
