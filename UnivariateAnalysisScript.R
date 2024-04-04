
# 2018

table(data_18$currdepress)

table(data_18$agegroup5)
table(data_18$birthsex)
table(data_18$newrace)
table(data_18$maritalstatus18)
table(data_18$insure18)
table(data_18$education)
table(data_18$didntgetcare18)
table(data_18$exercise18)
table(data_18$generalhealth)
table(data_18$employment18)

table(data_18$nutrition47)
table(data_18$sleepquality_q1)
table(data_18$enoughfood)
table(data_18$medcost18)

# 2019

table(data_19$nspd)
table(data_19$generalhealth)
table(data_19$agegroup5)
table(data_19$birthsex)
table(data_19$newrace)
table(data_19$maritalstatus19)
table(data_19$insure19r)
table(data_19$employment19)
table(data_19$education)
table(data_19$nutrition47)
table(data_19$didntgetcare19)
table(data_19$exercise19)

table(data_19$difficultdailyact)
table(data_19$cogdecline)
table(data_19$sexasltrape)

# 2020

table(data_20$nspd)

table(data_20$agegroup5)
table(data_20$birthsex)
table(data_20$newrace)
table(data_20$maritalstatus20)
table(data_20$insure20r)
table(data_20$employment20)
table(data_20$education)
table(data_20$nutrition47)
table(data_20$didntgetcare20)
table(data_20$exercise20)
table(data_20$generalhealth)

table(data_20$difficultdailyact)

table(data_20$insultipv)
table(data_20$avgsodasugarperday20)
table(data_20$twoplussoda)
table(data_20$delaypayrent)
table(data_20$heavydrink20)
table(data_20$wswexclusive)

mean(unfactor(data_18$nutrition47) + unfactor(data_18$nutrition46))
median(unfactor(data_18$nutrition47)+ unfactor(data_18$nutrition46))

mean(unfactor(data_19$nutrition47)+ unfactor(data_19$nutrition46))
median(unfactor(data_19$nutrition47)+ unfactor(data_18$nutrition46))

mean(unfactor(data_20$nutrition1))
median(unfactor(data_20$nutrition1))
