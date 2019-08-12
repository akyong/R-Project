data = read.csv('/Users/admin/Downloads/teams.csv')
attach(data)
str(data)
#plot(payroll,wins)
#scatterplot divided by league
l1 = which(league == 'NL')
l2 = which(league == 'AL')
plot(payroll[l1],wins[l1], xlim = range(payroll), ylim = range(wins), xlab = 'payroll', ylab = 'wins')
points(payroll[l2], wins[l2], pch=2)

p1 = which(pct > 0.5)
p2 = which(pct <= 0.5)
plot(payroll[p1], wins[p1], xlim = range(payroll), ylim = range(wins), xlab='payroll', ylab='wins')
points(payroll[p2],wins[p2],pch=2)
lines(range(payroll),c(80,80), lty=3)

plot(payroll~wins,pch = c(1,19), xlab = "payroll", ylab = "wins")
legend('topleft', c('league = AL', 'league = NL'), pch=c(1,19),title='Legend')

plot(payroll[as.numeric(league='1')],wins[as.numeric(league ="2")],  xlim = range(payroll), ylim = range(wins), xlab='payroll', ylab='wins')
legend('topleft', c('league = AL', 'league = NL'), pch=as.numeric(league),title='Legend')

plot(payroll~wins,pch=c(1,19))
with(data, text(wins, payroll, labels = code, pos = 1, cex=0.5))
legend('topleft', c('league =AL', 'league= NL'), pch=c(1,19),title='Legend')


#barplot
barplot(by(payroll,wins,sum))

#piechart
pie(by(as.numeric(payroll), league, sum))

#multiple diagram
par(mfrow=c(1,2)) #membagi 1 menjadi 2
n1 = which(league == 'NL')
n2 = which(league == 'AL')
plot(payroll[n1], wins[n1], main = 'NL', xlab = 'Payroll', ylab = 'Wins')
plot(payroll[n2], wins[n2], main = 'AL', xlab = 'Payroll', ylab = 'Wins')

library(ggplot2)
qplot(payroll,wins)




metro = read.csv('/Users/admin/Downloads/metropolitan.csv')
attach(metro)
str(metro)

#dimensi
dim(metro) #result 2759 (rows), 11(column)
nrow(metro) #2759 rows
ncol(metro) #11(column)
head(metro)
tail(metro)
summary(metro)


#show county only
metro.split = split(metro,metro$LSAD)
names(metro.split)

county = metro.split[['County or equivalent']]
head(county)

#box plot
boxplot(metro$BIRTHS2010 ~ metro$LSAD) # ~ --> dari setiap

#histogram
hist(county$BIRTHS2010)

#skewness adalah kecondongan , bagaimana memakai skewness
#kurtosis adalah  kayak gelombang atau parabola ~ 

plot(payroll)


survey_fixed = read.csv('/Users/admin/Downloads/survey_fixed.csv')
attach(survey_fixed)
str(survey_fixed)
#boxplot(survey_fixed) # ~ --> dari setiap
boxplot(as.numeric(survey_fixed$smoke))

survey_fixed.split = split(survey_fixed, survey_fixed$sex)
names(survey_fixed.split)

boxplot(survey_fixed.split) # ~ --> dari setiap


##
# tanggal 1 agustus 2019, kamis
# attach parenthood
attach(parenthood)
str(parenthood)
hist(dan.grump) #membuat histogram dari data dan.grump
hist(baby.sleep) #membuat histogram dari data baby sleep

plot(dan.grump, dan.sleep)
plot(baby.sleep)
plot(dan.grump)

#tau korelatif dari mana?
#pearson coeff corr

cor(dan.sleep, dan.grump)
cor(baby.sleep, dan.grump)
cor(baby.sleep, dan.sleep)

#feature extraction -> mencari mana yang paling mempengaruhi.

#attch effort

attach(effort)
str(effort)

hist(grade)
hist(hours)

plot(grade,hours)

#significan itu nilai yang semakin membentuk garis
cor(effort)
#korelation itu artinya memiliki hubungan

# \ adalah korelasi negatif karena datanya turun hingga ke negatif
# / adalah korelasi positif karena  datanya naik

cor(hours, grade, method ="spearman") #hasil -> 1 artiya absolute
#untuk corelation nilainya antara 0 ~ 1

#significan itu yang p-value < 0.05

#cor test (corelation test)
attach(parenthood)
cor.test(dan.sleep, dan.grump)
# Hasil
# Pearson's product-moment correlation

#data:  dan.sleep and dan.grump
#t = -20.854, df = 98, p-value < 2.2e-16
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# -0.9340614 -0.8594714
#sample estimates:
#      cor 
#-0.903384 


#cor = mendekati nilai 1 = memiliki korelasi 
#corelasi untuk mengecek apakah memilii korelasi
#cor.test= menggunakan p value , jika p value < 0,05 di anggap data SIGNIFIKAN
#korelasi test untuk mengecek significan atau ngak atau benar atau salah.

#autos.csv
auto = read.csv('/Users/admin/Downloads/autos.csv')
attach(auto)
str(auto)

cor(horsepower,price) #0.8124532
cor.test(horsepower,price) #p-value < 2.2e-16 = 0.00000000000000022

#regretion itu kayak peramalan.
#lm = liner model
model = lm(price ~ horsepower)
model
summary(model)
  
#prediksi harga mobil
dataframe_hpower = data.frame(horsepower = c(200,290,140,600))
dataframe_hpower
predict(model, dataframe_hpower)

#multiple variable linear regression
lm(price ~ length + engine.size + horsepower + city.mpg)

#100 adalah nilai length kita, 135 engine.size, 100 adalah horse power, 50 adalah city.mpg kita.
pred_price = 114.58 * 100 + 115.32*135 + 52.74 * 100 + 61.51 * 50 -28480 #-28480 adalah nilai intercept, 
#nilai intercept bisa dilihat dari jalankan coding lm (linear model)

#y = m * x + intecept
#m adalah yang mau diprediksi, x adalah variabel, misalnya saya ada 4 nilai yang mau saya tampung jadi ada 4 variable
pred_price

## --- TUGAS 3 --- 
#Nomor 1
pairs(parenthood)
cor(parenthood)
cor.test(baby.sleep, dan.grump)

#Nomor 2
auto = read.csv('/Users/admin/Downloads/autos.csv')
attach(auto)
str(auto)

cor(wheel.base,price) #0.5849506
cor.test(wheel.base,price) #p-value < 2.2e-16 = 0.00000000000000022

#regretion itu kayak peramalan.
#lm = liner model
model = lm(price ~ wheel.base)
model
summary(model)

#prediksi ukuran Roda
dataframe_wheelbase = data.frame(wheel.base = c(80,90,100,120))
dataframe_wheelbase
predict(model, dataframe_wheelbase)

#multiple variable linear regression
lm(wheel.base ~ length + engine.size + horsepower + price)

#100 adalah nilai length kita, 135 engine.size, 100 adalah horse power, 50 adalah city.mpg kita.
prediksi_ukuran_roda = 114.58 * 100 + 115.32*135 + 52.74 * 100 + 61.51 * 50 -28480 #-28480 adalah nilai intercept, 
#nilai intercept bisa dilihat dari jalankan coding lm (linear model)

pairs(auto)
pairs.default(auto)

#tanggal 7 Agustus 2019 RAbu 
#pakai data grump kemarin
attach(sleep)
sleep
mean(extra) #result -> 1.54

# H0 tidak ada perbedaan yang signifikan
# H1 ada perbedaan signifikan terhadap waktu tidur

# H0 diterima jika... p > 0.05
# H1 diterima jika... p < 0.05

# Significan (p < 0.05)(H1)
# Significan (p > 0.05)(H0)
#

t.test(extra, mu=0)


# t-test w/ dependent sample (paired t-test)
mean(extra[group==1]) #result -> 0.75
mean(extra[group==2]) #result -> 2.33

# ~~~~~~~~~ ---> "terhadap"
t.test(extra ~ group, sleep, var.equal = T, paired = T)


# t-test w/ independent sample
attach(harpo)
harpo


#Tugas 4
# No.1 Tunjukan proses pengujian paried samples t-test untuk data "chico.Rdata" yang ada pada link
# berikut http://bit.do/binus_itrm
attach(chico)
chico
mean(grade_test1) #56.98
mean(grade_test2) #58.385
total = grade_test1 + grade_test2
mean(total)
t.test(total)


# No.2 Tunjukan proses pengujian anova untuk data "clinicaltrial.Rdata" yang ada pada link 
# berikut http://bit.do/binus_itrm
attach(clin.trial)
clin.trial
str(clin.trial)

anovaTest=aov(mood.gain~therapy*drug)
TukeyHSD(anovaTest)


