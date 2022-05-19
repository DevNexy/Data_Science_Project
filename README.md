## Data_Science_Project (데이터 사이언스 프로젝트)
## 2021-1 Data_Science_Basic final project 
## (2021년도 1학기 데이터 사이언스 기초 과목 기말 프로젝트)
---
### [Q1] [200점] - lm 을 이용한 단일회귀분석, 다중회귀분석 적용
<사용되는 데이터 - Pre_Season_Batter.csv>   
Pre_Season_Batter.csv 데이터를 가지고 단일회귀분석과 다중회귀분석을 적용 및 예측하자   
Pre_Season_Batter.csv는 결측값을 가지고 있어 시작전에 아래의 코드를 적용하고 진행하세요.   
![image](https://user-images.githubusercontent.com/92451281/169382571-d41594c0-0ba3-4b6d-bec5-d68e05458037.png)

<참고사항>
1. 데이터는 Pre_Season_Batter.csv 데이터를 사용한다.
2. 분석하고자 하는 Team은 KIA, 롯데, LG 팀중 한팀만 선택하는 것으로 제한한다.
3. 반은변수(종속변수)는 R 득점으로 제한하고 설명변수(독립변수)들은 자유롭게 선택한다.
4. 단일회귀 분석에서는 서로 다른 설명변수를 가지는 모델 2개를 생성한다.
5. 다중회귀분석에서는 설명변수를 2개를 가지는 모델 2개를 생성한다.
6. 사용하는 설명변수에 대한 시각화 및 설명, 회귀선 또는 회귀평면이 포함된 시각화 및 설명, 모델적합(summary 결과 해석)에 대한 설명 포함되어야 함
7. 모델 예측은 임의의 수 또는 난수를 이용한 예측이 반드시 포함되어야 함
8. 시각화의 경우 base R, PerformanceAnalytics, scatterplot3d 등을 사용할 수 있음
9. 단일회귀분석: 사용하는 변수에 대한 시각화 및 설명(30점), 회귀선이 포함된 시각화 및 설명(30점), 모델적합에 대한 설명(30점), 예측(10점)
10. 다중회귀분석: 사용하는 변수에 대한 시각화 및 설명(30점), 회귀평면이 포함된 시각화 및 설명(30점), 모델적합에 대한 설명(30점), 예측(10점)   
---
<풀이>   
먼저 Pre_Season_Batter.csv 데이터를 사용하였고, 데이터 전처리를 해주었습니다.
```R
library(tidyverse)
library(data.table)

DF <- fread("Pre_Season_Batter.csv") %>% as_tibble()
```
```R
#데이터 전처리
input <-DF %>% na.omit()%>%
  select(-`height/weight`, -year_born, -position,-career, -starting_salary)
input <- input %>% mutate(H = H -`2B` - `3B` - HR)
```
분석하고자 하는 팀을 "롯데"팀으로 설정하였습니다.
```R
# 분석하고자 하는 Team은 KIA, 롯데, LG팀중 한팀만 선택하는 것으로 제한한다.
input <- input %>% filter(team == "롯데")
View(input)
```
단일회귀분석 첫번째 모델 : m1, 두번째 모델: m2 반응변수(종속변수)는 R득점으로 제한하고 설명변수(독립변수)들은 각각 HR, H로 선택하였습니다.
```R
# --단일회귀분석 2개의 모델
# 반응변수(종속변수)는 R득점으로 제한하고 설명변수(독립변수)들은 자유롭게 선택한다.
m1 <- lm(formula = R ~ HR, data = input)
m1

m2 <- lm(formula = R ~ H, data = input)
m2
```
m1에서 R = 1.630 + 1.605 * HR 라는 회귀식을 도출할 수 있고,
m2에서 R = 0.7405 + 0.5517 * H라는 회귀식을 도출할 수 있습니다.   
![image](https://user-images.githubusercontent.com/92451281/169387702-26d7a7c3-46d6-47d6-afda-0e929cd4188e.png)
![image](https://user-images.githubusercontent.com/92451281/169387714-887e8d6e-cb8a-4977-b0bc-2f99372a76b3.png)   
각각 시각화를 해보면,  
```R
ggplot(input, aes(x = HR, y = R))+
  geom_point(color = "tomato")+
  geom_abline(intercept = coef(m1)[1], slope = coef(m1)[2], color = "blue", size = 1.5)
summary(m1)

ggplot(input, aes(x = H, y = R))+
  geom_point(color = "green")+
  geom_abline(intercept = coef(m2)[1], slope = coef(m2)[2], color = "blue", size = 1.5)
summary(m2)
```
다음과 같이 나옵니다. 여기에서 X축은 HR, Y축은 R을 가리키고, R은 반응 변수, HR은 설명 변수 입니다.   
![image](https://user-images.githubusercontent.com/92451281/169388259-2d88c55d-f6c2-4a4d-ac3a-174c7621f6f2.png)
![image](https://user-images.githubusercontent.com/92451281/169388394-27360c93-f347-4933-9c4b-31e4a8d2bfe2.png)   
<m1 모델 해석>
1. F-statistic의 p-value 값은 5.702e-12로 0.05보다 작기에 이 회귀식은 회귀분석 모델 전체에 대해 통계적으로 의미가 있다고 볼 수 있습니다.
2. 중간의 Coefficients: 에는 y절편 값(Intercept) 및 변수들의 p-value 값이 나와있습니다.(HR 변수의 경우 5.7e-12로 0.05보다 작기에 R을 설명하는데 유의하다가 판단할 수 있습니다.
3. Adjusted R-squared 값은 0.2553으로, 26%만큼의 설명력을 가진다고 판단할 수 있습니다.   

여기에서 X축은 H, Y축은 R을 가리키고, R은 반응 변수, H은 설명 변수 입니다.
![image](https://user-images.githubusercontent.com/92451281/169388619-e6028f25-c480-45e8-9039-ec023c53fd52.png)
![image](https://user-images.githubusercontent.com/92451281/169388752-5da62c85-8ebc-41cf-b08e-e0acaf31f0d2.png)   
<m2모델 해석>
1. F-statistic의 p-value 값은 2.2e-16로 0.05보다 작기에 이 회귀식은 회귀분석 모델 전체에 대해 통계적으로 의미가 있다고 볼 수 있습니다.
2. Coefficients: 에는 y절편 값(Intercept) 및 변수들의 p-value 값이 나와있습니다.(H 변수의 경우 2e-16로 0.05보다 작기에 R을 설명하는데 유의하다가 판단할 수 있습니다.
3. Adjusted R-squared 값은 0.3538으로, 35%만큼의 설명력을 가진다고 판단할 수 있습니다.   

난수를 생성하여 두 모델을 예측해 보겠습니다.
```R
#난수 생성
set.seed(31)

rand <- sample(1:40, 10, replace = F)

random_m1 <- tibble(HR = rand)
random_m2 <- tibble(H = rand)

View(random_m1)
```
```R
#득점 예측
result_DF <- bind_cols(tibble(value = rand),
                       tibble(HR_predict = predict(m1, random_m1)),
                       tibble(H_predict = predict(m2, random_m2)))
View(result_DF)
```
![image](https://user-images.githubusercontent.com/92451281/169389445-837e84fe-c800-46ed-be54-d1da9516b336.png)   
예측값이 각각 HR_predict, H_predict 값으로 도출이 되고, 이를 시각화 해보면,
```R
ggplot(input, aes(x = HR, y= R))+
  geom_point(data = result_DF, aes(x = value, y = HR_predict), color = "tomato", size = 3.5)+
  geom_abline(intercept = coef(m1)[1], slope = coef(m1)[2], color = "blue", size = 1.5, alpha = 0.3)+
  scale_y_continuous(limits = c(0,100))

ggplot(input, aes(x = H, y= R))+
  geom_point(data = result_DF, aes(x = value, y = H_predict), color = "green", size = 3.5)+
  geom_abline(intercept = coef(m2)[1], slope = coef(m2)[2], color = "blue", size = 1.5, alpha = 0.3)+
  scale_y_continuous(limits = c(0,100))
```
![image](https://user-images.githubusercontent.com/92451281/169389584-075b2b68-914d-4c63-8759-cacef4fce4c3.png)
![image](https://user-images.githubusercontent.com/92451281/169389589-c8cbcb2d-f3ea-442e-8a3e-79329d66b9e0.png)   
이러한 결과가 나옴으로써. 예측한 값이 모두 회귀선안에 들어와 있기 때문에 예측이 잘 된것으로 볼 수 있습니다.   
   
다중회귀분석 첫번째 모델 : m3, 두번째 모델: m4 반응변수(종속변수)는 R득점으로 제한하고 설명변수(독립변수)들은 각각 HR과 H, AB+G로 선택하였습니다.
```R
# --다중회귀분석 2개의 모델

library(scatterplot3d)

m3 <- lm(R ~ HR + H, data = input)
m3

m4 <- lm(R ~ AB + G, data = input)
m4
```
![image](https://user-images.githubusercontent.com/92451281/169390613-abf9928f-1633-4fcb-a581-68676685bf06.png)
![image](https://user-images.githubusercontent.com/92451281/169390640-d2925a8d-2692-4af3-a5b5-4af6fbadb241.png)   
M3에서 R = 0.5161 + 1.2247 * HR + 0.4658*H 라는 회귀식을 도출할 수 있고,   
M4에서 R = 0.12890 + 0.14768 * AB+-0.02398*G라는 회귀식을 도출할 수 있습니다.   
![image](https://user-images.githubusercontent.com/92451281/169390812-840a43c6-1a84-470a-b235-6908f6be4f2f.png)   
<m3 모델 해석>   
1. F-statistic의 p-value 값은 2.2e-16으로 0.05보다 작기에 이 회귀식은 회귀분석 모델 전체에 대해 통계적으로 의미가 있다고 볼 수 있습니다.
2. Coefficients: 에는 y절편 값(Intercept) 및 변수들의 p-value 값이 나와있습니다. 모든 설명변수가 0.05보다 작기에 R을 설명하는데 유의하다가 판단할 수 있습니다.
3. Adjusted R-squared 값은 0.4942으로, 49%만큼의 설명력을 가진다고 판단할 수 있습니다.   

![image](https://user-images.githubusercontent.com/92451281/169391168-a1661b36-9e92-4b9a-ab27-16cd19be7bc6.png)

<m4 모델 해석>
1. F-statistic의 p-value 값은 2.2e-16으로 0.05보다 작기에 이 회귀식은 회귀분석 모델 전체에 대해 통계적으로 의미가 있다고 볼 수 있습니다.
2. Coefficients: 에는 y절편 값(Intercept) 및 변수들의 p-value 값이 나와있습니다.    AB설명변수가 0.05보다 작기에 R을 설명하는데 유의하다가 판단할 수 있습니다.   
G 설명변수가 0.725로 0.05보다 크기에 R을 설명하는데 유의하지 않다고 판단할 수 있습니다.
3. Adjusted R-squared 값은 0.4094으로, 41%만큼의 설명력을 가진다고 판단할 수 있습니다.   

다음으로 m3모델을 시각화해보면, 여기에서 X축은 HR, Y축은 H을 가리키고, Z축은 R을 가리키며, R은 반응 변수, HR, H는 설명 변수 입니다.   
![image](https://user-images.githubusercontent.com/92451281/169391588-c08a7ab5-66a0-4899-8ec6-4c581b366539.png)   
m4모델을 시각화해보면, 여기에서 X축은 AB, Y축은 G를 가리키고, Z축은 R을 가리키며, R은 반응 변수, AB, G는 설명 변수 입니다.
![image](https://user-images.githubusercontent.com/92451281/169391826-71e03bf1-8846-4d87-bc58-95bcdf185ddf.png)   
예측을 위해 난수를 생성하여, 새로운 변수에 넣어주었습니다.
```R
#난수 생성
set.seed(31)
HR_r <- sample(min(input$HR):max(input$HR), 5)
H_r <- sample(min(input$H):max(input$H), 5)
AB_r <- sample(min(input$AB):max(input$AB), 5)
G_r <- sample(min(input$G):max(input$G), 5)

new_m3 <- tibble(HR = HR_r, H = H_r)
new_m4 <- tibble(AB = AB_r, G = G_r)
```
다음과 같은 코드를 통해 각각 m3, m4 의 R_predict값으로 예측을 할 수 있습니다.
```R
#새로운 데이터 예측
m3_R_r <- predict(m3, new_m3)
m4_R_r <- predict(m4, new_m4)

new_m3 <- new_m3 %>% bind_cols(tibble(R_predict = m3_R_r))
View(new_m3) %>% show()

new_m4 <- new_m4 %>% bind_cols(tibble(R_predict = m4_R_r))
View(new_m4) %>% show()
```
![image](https://user-images.githubusercontent.com/92451281/169392515-168a50d6-64a8-44fd-9b6f-59b4799c62a5.png)
![image](https://user-images.githubusercontent.com/92451281/169392548-5a680cef-f30f-4960-9407-cf9e53368372.png)   
이를 시각화 해보면, 빨간색 히스토그램이 예측값이며, M3 모델은 몇 개의 값만 예측이 잘 된 것을 볼 수 있습니다.   
![image](https://user-images.githubusercontent.com/92451281/169392651-da0231f9-23a1-4368-8a16-3b7641fa0fba.png)   
다음 시각화에서 빨간색 히스토그램이 예측값이며, M4 모델은 예측이 잘 되지 않은 것을 볼 수 있습니다.
![image](https://user-images.githubusercontent.com/92451281/169393045-ec9cd3d2-221f-49e1-afd0-59f3fb0b0591.png)

---
### [Q2] [100점] - glm 을 이용한 로지스틱 회귀분석
<사용되는 데이터 – survival 라이브러리의 colon 데이터셋>   
colon 데이터셋을 이용하여 로지스틱 회귀분석을 해보자!   
Colon 데이터셋 다운로드 방법   
![image](https://user-images.githubusercontent.com/92451281/169385730-92094089-db05-406d-b76e-3a48d0561a16.png)   
<참고사항>   
1. Colon 데이터를 로드 후 다음과 같은 전처리 과정을 거친다.
![image](https://user-images.githubusercontent.com/92451281/169384684-f53324e9-e96d-46c9-a1b2-78a629e3ceb5.png)
2. clean_colon 데이터를 caret 패키지의 createDataPartition 을 이용하여 train/ test 으로 분리할 것(비율은 9:1 로 할 것)
3. 시각화의 경우 ggplot2, base R, ggarrange, chart.Correlation 등 자유롭게 사용할 것 시각화 결과를 반영하여 설명변수를 선택할 것.
4. Train dataset 을 사용하고glm 함수를 사용하여 모델을 생성하고 반응변수는 status로 설정한다, 설명변수는 설명변수의 p-value가 0.05를 초과하지 않는 모든 것을 사용한다.   
i.	만약, 설명변수의 p-value가 0.05를 초과할 경우 step, update을 사용하여 해당 설명변수를 제외한 모델을 새롭게 정의할 것. 새롭게 정의한 모델의 summary결과를 확인한다.
5. Test set을 사용해서 모델을 예측한다. 예측 결과와 원본 데이터와 비교하여 모델의 적합성에 대하여 기술한다.
6.	시각화 및 분석 설명(40점), 모델적합에 대한 설명(40점), 예측(20점) – 각 과정은 이론, 실습 영상을 참고할 것.
---
