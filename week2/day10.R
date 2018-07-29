#18.7.13

getwd()

idol <- read.table("idol.txt",header=T)

attach(idol) #활성화시작

boxplot(RV, GF, BP, BTS, WO, EXO, names=c("RV", "GF", "BP", "BTS", "WO", "EXO"), col = rainbow("6"))

t.test(EXO,mu=3)

var(EXO)

sqrt(var(EXO))

#여기서 양쪽 끝에 일 확률을 계산했더니 = (클수도 있고, 작을수도 있는것)
#뮤가 3일 때 나올 확률이 안된다. -> 기각 

t.test(EXO,mu=2.8)

# 2.8일때 p값은 가능성이 있다 


#t 값은 똑같고, 이번에는 아래쪽 작은쪽만 고려하겠다. 그러면 p값이 반토막난다.
t.test(EXO,mu=3,alternative=c("less"))
 
#이번엔 큰쪽만 -> 
t.test(EXO,mu=3,alternative=c("greater"))


mean(WO)
t.test(WO,mu=3.3)
t.test(WO,mu=3.3, alternative=c("greater"))
t.test(WO,mu=3.3, alternative=c("less"))
t.test(WO,mu=3.8)

#WO는 3.3에서 올라가는데 채택된다


#둘을 비교해보고싶다.
#먼저 분산이 둘이 같으냐 다르냐가 중요하다.
#분산테스트 -> 가설은 같다.or차이가 =0
var.test(EXO,WO)
#같을 확률이 98%,  F=1 로간다

# EXO의 평균과WO의 평균을 빼면 0것에 가까울것. 
t.test(EXO,WO,var.equal=T)
# 호감도가 같은 것은 거의 불가

# EXO가 WO보다 작다고 생각하는가?
t.test(EXO,WO,var.equal=T,alternative=c("less"))

# p값이 1 같고 클리는없다
t.test(EXO,WO,var.equal=T,alternative=c("greater"))



#그림으로 척도를 그려보자
boxplot(dance_i,dance_p, sing_i, sing_p, variety_i, variety_p, person_i, person_p, plan_i, plan_p,capital_i,
        capital_p, marketing_i, marketing_p,
       names=c('dance_i','dance_p', 'sing_i', 'sing_p', 'variety_i', 'variety_p', 
              'person_i', 'person_p', 'plan_i','plan_p','capital_i','capital_p', 'marketing_i', 'marketing_p'))


mean(sing_i); mean(sing_p);
# 아이돌이 성공하기위해 노래를 잘하는건 중요하다고 생각하지만, 기대보단 떨어지는 점수
# 알고싶은것은 이 차이가 실제로 의미있는 차이인가를 알아보는 것


var.test(sing_i,sing_p)
# 분산이 같은가? > 같다 , 그렇다면 테스트에서  equal 사용

t.test(sing_i,sing_p,var.equal=T)
# 분산이 같을때, 같을 가능성은 0= 다르다
# 우리가 볼때 노래의 중요도와 실행도에는 차이가 있다.
# 차이는 0.2~0.6정도의 차이가 있다.
# sing_i - sing_p > 0 == 기대에는 못미친다.


t.test(sing_i,sing_p,var.equal=T,alternative=c("greater"))
# 차이가 같다에 대해서 큰쪽으로 테스트인데, p가0에 가까우므로 기각, 
# 그러므로 H1 sing_i>sing_p ->유의미한 차이이다.

t.test(sing_i,sing_p,mu=0.5, var.equal=T,alternative=c("greater")) #0.7 or 0.2
#mu =0.5 : 차이가 0.5정도 되는가?

t.test(sing_i,sing_p,mu=0.7, var.equal=T,alternative=c("greater"))
#0.7도 받아들일만한 수치

t.test(sing_i,sing_p,mu=0.2, var.equal=T,alternative=c("greater"))
# 0.2 는 기가

t.test(sing_i,sing_p,mu=0.9, var.equal=T,alternative=c("greater"))
# 0.9도 받아드림

#결론> 기대에 실제값이 못미치고 (최소0.5) ~ 최대 0.9점까지 차이가 있는것 같습니다.


t.test(dance_i,dance_p,var.equal=T)
# 0.1정도의 차이는 받아들인다.
#기대에 비해 높게 평가해주는것으로 받아들일 수 있다.

t.test(dance_i,dance_p,pairs=T)
#pairs : 두 집단이 동일하지 않을때 





###회귀분석

#관계 그리기
plot(boy,girl)

#상관관계 
cor(boy,girl)


idol2 <- idol[!idol$boy==0,]
attach(idol2)
plot(boy,girl)
cor(boy,girl)
detach(idol2)

#보이그룹의 선호도에 미치는 영향을 알고싶다


blm <- lm(boy~dance_i+sing_i+variety_i+person_i+plan_i+capital_i+marketing_i)
blm
summary(blm)

glm <- lm(girl~dance_i+sing_i+variety_i+person_i+plan_i+capital_i+marketing_i)
glm
summary(glm)

#의미 없는것을 빼버리고
blm <- lm(boy~dance_i+sing_i+variety_i+plan_i)
blm


#컴퓨터가 알아서 잘라주는 작업
blm <- step(lm(boy~dance_i+sing_i+variety_i+person_i+plan_i+capital_i+marketing_i),direction="both")#backward,foreward
summary(blm)

#컴퓨터가 선택한것으로 돌려보자
blm <- lm(boy ~ dance_i + sing_i + variety_i + person_i + plan_i)
summary(blm)



blm <- step(lm(boy~dance_p+sing_p+variety_p+person_p+plan_p+capital_p+marketing_p),direction="both")
blm <- lm(boy ~ dance_p + sing_p + variety_p + capital_p + marketing_p)
summary(blm)


#그룹이 3개 이상이면 t-test의 효율이 떨어짐 (기본이 2그룹)
#

#anova

anova(lm(boy~type)) #가설은 차이없다. 채택
boxplot(boy~type)


anova(lm(boy~Gender))#오류 anova는 집단이 3개이상
t.test(boy~Gender)


boxplot(boy~sing_i)
anova(lm(boy~sing_i))

#pairwise.t.test = 3묶음이 나오는것을 비교함 하나씩어느것 까지 차이가 나는지
pairwise.t.test(boy, sing_i)
oneway.test(boy~sing_i)
pairwise.t.test(boy, sing_i, pool.sd=F) #pool.sd=합동분산, 분산이 다르다고 가정하겠다.
