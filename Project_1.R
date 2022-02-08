getwd()
library(tidyverse)
library(data.table)

DF <- fread("Pre_Season_Batter.csv") %>% as_tibble()

#데이터 전처리
input <-DF %>% na.omit()%>%
  select(-`height/weight`, -year_born, -position,-career, -starting_salary)
input <- input %>% mutate(H = H -`2B` - `3B` - HR)

# 분석하고자 하는 Team은 KIA, 롯데, LG팀중 한팀만 선택하는 것으로 제한한다.
input <- input %>% filter(team == "롯데")
View(input)

# --단일회귀분석 2개의 모델
# 반응변수(종속변수)는 R득점으로 제한하고 설명변수(독립변수)들은 자유롭게 선택한다.
m1 <- lm(formula = R ~ HR, data = input)
m1

m2 <- lm(formula = R ~ H, data = input)
m2

ggplot(input, aes(x = HR, y = R))+
  geom_point(color = "tomato")+
  geom_abline(intercept = coef(m1)[1], slope = coef(m1)[2], color = "blue", size = 1.5)
summary(m1)

ggplot(input, aes(x = H, y = R))+
  geom_point(color = "green")+
  geom_abline(intercept = coef(m2)[1], slope = coef(m2)[2], color = "blue", size = 1.5)
summary(m2)

#난수 생성
set.seed(31)

rand <- sample(1:40, 10, replace = F)

random_m1 <- tibble(HR = rand)
random_m2 <- tibble(H = rand)

View(random_m1)

#득점 예측
result_DF <- bind_cols(tibble(value = rand),
                       tibble(HR_predict = predict(m1, random_m1)),
                       tibble(H_predict = predict(m2, random_m2)))
View(result_DF)

ggplot(input, aes(x = HR, y= R))+
  geom_point(data = result_DF, aes(x = value, y = HR_predict), color = "tomato", size = 3.5)+
  geom_abline(intercept = coef(m1)[1], slope = coef(m1)[2], color = "blue", size = 1.5, alpha = 0.3)+
  scale_y_continuous(limits = c(0,100))

ggplot(input, aes(x = H, y= R))+
  geom_point(data = result_DF, aes(x = value, y = H_predict), color = "green", size = 3.5)+
  geom_abline(intercept = coef(m2)[1], slope = coef(m2)[2], color = "blue", size = 1.5, alpha = 0.3)+
  scale_y_continuous(limits = c(0,100))

# --다중회귀분석 2개의 모델

library(scatterplot3d)

m3 <- lm(R ~ HR + H, data = input)
m3

m4 <- lm(R ~ AB + G, data = input)
m4

summary(m3)
summary(m4)

m3_s <- scatterplot3d(input$HR, input$H, input$R, pch = 20, type = 'h',angle = 55, color = "black")
m3_s$plane3d(m3)

m4_s <- scatterplot3d(input$AB, input$G, input$R, pch = 20, type = 'h',angle = 55, color = "black")
m4_s$plane3d(m4)

#난수 생성
set.seed(31)
HR_r <- sample(min(input$HR):max(input$HR), 5)
H_r <- sample(min(input$H):max(input$H), 5)
AB_r <- sample(min(input$AB):max(input$AB), 5)
G_r <- sample(min(input$G):max(input$G), 5)

new_m3 <- tibble(HR = HR_r, H = H_r)
new_m4 <- tibble(AB = AB_r, G = G_r)

#새로운 데이터 예측
m3_R_r <- predict(m3, new_m3)
m4_R_r <- predict(m4, new_m4)

new_m3 <- new_m3 %>% bind_cols(tibble(R_predict = m3_R_r))
View(new_m3) %>% show()

new_m4 <- new_m4 %>% bind_cols(tibble(R_predict = m4_R_r))
View(new_m4) %>% show()

#새로운 데이터 시각화
m3_s <- scatterplot3d(input$HR, input$H, input$R, pch = 20, type = 'h',angle = 55, color = "black")
m3_s$plane3d(m3)
m3_s$points3d(new_m3$HR, new_m3$H, new_m3$R, col = "red", type = "h", pch = 16, cex = 1.5)

m4_s <- scatterplot3d(input$AB, input$G, input$R, pch = 20, type = 'h',angle = 55, color = "black")
m4_s$plane3d(m4)
m4_s$points3d(new_m4$AB, new_m4$G, new_m4$R, col = "red", type = "h", pch = 16, cex = 1.5)
