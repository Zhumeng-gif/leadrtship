use "G:\workplace\为村数据\Results\dtaData\dataVillage.dta", clear

// ======== 1. activity var. set ========
// index of a county's online activity, based on WeiCounty documentations.
gen activeness = blogCnt * 0.2 + cmtCnt * 0.25 + likeCnt * 0.45 + clickCnt * 0.1
gen circleAct = blogCnt_circle * 0.2 + cmtCnt_circle * 0.25 + likeCnt_circle * 0.45 + clickCol_circle * 0.1
gen electionAct = blogCnt_election * 0.2 + cmtCnt_election * 0.25 + likeCnt_election * 0.45 + clickCol_election * 0.1
gen affairAct = blogCnt_villageAffair * 0.2 + cmtCnt_villageAffair * 0.25 + likeCnt_villageAffair * 0.45 + clickCol_villageAffair * 0.1
gen revitalAct = blogCnt_revitalize * 0.2 + cmtCnt_revitalize * 0.25 + likeCnt_revitalize * 0.45 + clickCol_revitalize * 0.1
gen lnAct = ln(activeness)
gen lnCircleAct = ln(circleAct+1)
gen lnElectionAct = ln(electionAct+1)
gen lnAffairAct = ln(affairAct+1)
gen lnRevitalAct = ln(revitalAct+1)


// ======== 2. user structure var. set ========
// number of registered users in given county.
gen userCnt = vpreCreate + vonCreate + vpostCreate
// self-reported gender ratio
gen femRatio = female/ userCnt
// cadre-masses ratio
gen cadre = userParty / userCnt
// representities-masses ratio
gen rep = userComm / userCnt

// median of each attr. of each village
egen femHalf = median(femRatio)
egen cadreHalf = median(cadre)
egen repHalf = median(rep)

// cate. each village by its attr.
gen femVlg = 1 if femRatio > femHalf
gen cadreVlg = 1 if cadre > cadreHalf
gen repVlg = 1 if rep > repHalf
replace femVlg = 0 if femVlg == .
replace cadreVlg = 0 if cadreVlg == .
replace repVlg = 0 if repVlg == .
label define cadreVlg 0 "非党员村" 1 "党员村"
label define femVlg 0 "非巾帼村" 1 "巾帼村"
label define repVlg 0 "非干部村" 1 "干部村"
label values cadreVlg cadreVlg
label values repVlg repVlg
label values femVlg femVlg

// ======== 3. eco potential var. set ========
gen lnNL = ln(nightlight)
gen isPSC = 1 if exitYear != . // is poverty striken county
replace isPSC = 0 if exitYear == .

// median of each attr. of each village
egen NLHalf = median(lnNL)
gen welloffVlg = 1 if lnNL > NLHalf
replace welloffVlg = 0 if welloffVlg == .
label define welloffVlg 0 "非小康村" 1 "小康村"
label define contract 0 "非动员村" 1 "动员村"
label define isPSC 0 "非摘帽村" 1 "摘帽村"
label values welloffVlg welloffVlg
label values contract contract
label values isPSC isPSC

// ======== 4. political potential var. set =======
//tab contract

// ======== 5. con. var. set ========
gen lastYear = 2022-create_year // how long does given country last.
egen cityCnt = count(wid), by(city)
replace cityCnt = ln(cityCnt)
egen cityAct = mean(lnAct), by(city)

// no interaction or users indicates zombie village
gen zombie=1 if activeness==0 | userCnt==0
replace zombie=0 if zombie==.

label define dichotomous 1 "yes" 0 "no"
// label values contract dichotomous
// label values isPSC dichotomous
label values zombie dichotomous


// ======== 6. reg =======
// reg lnAct lnNL userCnt i.isPSC i.contract // reg without excluding zombie Vs.
drop if zombie == 1 // exclude zombie villages.

reg lnAct userCnt femRatio cadre rep lnNL isPSC i.contract lastYear
// shapley2, stat(r2) group(userCnt femRatio, cadre rep, lnNL isPSC, contract, lastYear)

// 集束化
// sheafcoef, latent(test: cadre rep)


// ======== 7. visualize =======
