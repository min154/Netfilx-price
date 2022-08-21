pacman::p_load(ggplot2, tidyverse)

'C:/Rworkspace/data/Netflix price in different countries.csv' %>%
  read.csv() %>%
  as_tibble() -> netflix

str(netflix)


#열이름이 지저분하니까 간단하게 바꾸기

netflix %>% rename(country = Country,
                   total = Total.Library.Size,
                   tv_shows = No..of.TV.Shows,
                   movies = No..of.Movies,
                   basic = Cost.Per.Month...Basic....,
                   standard = Cost.Per.Month...Standard....,
                   premium = Cost.Per.Month...Premium....) -> netflix


str(netflix)



#서비스 중인 콘텐츠 수와 구독료 사이 관계가 있을까?
  #basic 구독료가 9.03$로 같은 국가들이 유독 많으니 콘텐츠 수를 비교

  netflix %>% select(basic) %>% table()

  netflix %>% filter(basic == 9.03) %>%
    ggplot(aes(x = country %>% fct_reorder(total), y = total)) + 
    geom_col() + geom_text(aes(label = total), nudge_y = 300) + coord_flip() +
    labs(x = "국가명", y = "서비스 중인 총 콘텐츠 수")

  #콘텐츠 수와 구독료(standard 기준) 산점도

  netflix %>% ggplot(aes(x = total, y = standard)) + geom_point() +
    labs(x = "서비스 중인 총 콘텐츠 수", y = "스탠다드 구독료")



#콘텐츠 수 대비 구독료가 가장 비싼 국가와 가장 저렴한 국가

netflix %>%
  transmute(country, cost_per_contents = standard / total) %>% 
  arrange(-cost_per_contents) %>% head(1) -> most_expensive

netflix %>%
  transmute(country, cost_per_contents = standard / total) %>%
  arrange(-cost_per_contents) %>% tail(1) -> cheapest

most_expensive %>% select(cost_per_contents) / cheapest %>% select(cost_per_contents) #약 10.4배 차이



#각 변수별 평균과 우리나라 수치 비교

netflix %>% summarise(across(-country, ~sum(.x) / nrow(netflix)))
netflix %>% filter(country == 'South Korea')

netflix_df_contents <- tribble(~list, ~total, ~tv_shows, ~movies,
                               'mean', 5314, 3519, 1795,
                               'South Korea', 5195, 3334, 1861)

netflix_df_costs <- tribble(~list, ~basic, ~standard, ~premium,
                            'mean', 8.37, 12.0, 15.6,
                            'South Korea', 8.07, 11.5, 14.4)

#5장에 나오는 pivot_longer 함수 이용
netflix_df_contents %>% 
  pivot_longer(cols = total:movies, names_to = 'contents', values_to = 'number') -> netflix_df_contents

netflix_df_costs %>%
  pivot_longer(cols = basic:premium, names_to = 'subscribe', values_to = 'cost') -> netflix_df_costs

netflix_df_contents$contents = factor(netflix_df_contents$contents,levels = c("total","tv_shows","movies"))
netflix_df_costs$subscribe = factor(netflix_df_costs$subscribe,levels = c("basic","standard","premium"))

netflix_df_contents %>% 
  ggplot(aes(x = list, y = number)) + geom_col() + facet_grid( ~ contents) +
  geom_text(aes(label = number), nudge_y = 300) + labs(x = "평균과 우리나라 비교", y = "서비스 중인 콘텐츠 수")

netflix_df_costs %>%
  ggplot(aes(x = list, y = cost)) + geom_col() + facet_grid( ~ subscribe) +
  geom_text(aes(label = cost), nudge_y = 1) + labs(x = "평균과 우리나라 비교", y = "멤버십 구독료")

