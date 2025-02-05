
use "individual_rf_panel.dta", clear

merge m:1 month year reside using "energy_prices_malawi.dta"
keep if _m == 3
drop _m

tab sex
tab sex, nolabel

recode sex (1 = 0) (2 = 1)
label define sex 0 "Male" 1 "Female"
label values sex sex

summ age, det

tab adult

g adult_female = 0
replace adult_female = 1 if adult == 1 & sex == 1

g adult_male = 0
replace adult_male = 1 if adult == 1 & sex == 0

tab education adult, column
tab education adult, nolabel

tab reside
tab reside, nolabel
recode reside (2 = 0)

label define res 0 "Rural Household" 1 "Urban Household"
label values reside res

tab own_solar_panel
tab own_solar_panel, nolabel

label define solar 0 "No Solar Panel" 1 "Owns Solar Panel"
label values own_solar_panel solar

label define education 0 "None" 1 "Primary" 2 "Lower Secondary" 3 "Upper Secondary" 4 "College \& Above"
label values education education
tab education

tab literacy_chichewa
tab literacy_chichewa, nolabel

label define lit_ch 0 "Not literate in Chichewa" 1 "Literate in Chichewa"
label values literacy_chichewa lit_ch
tab literacy_chichewa

tab literacy_english
tab literacy_english, nolabel

label define lit_eng 0 "Not literate in English" 1 "Literate in English"
label values literacy_english lit_eng
tab literacy_english

tab whether_fwood_purchased
tab whether_fwood_purchased, nolabel

drop if hours_year_firewood_collect ==. // missing observations
count if hours_year_paid_job == 0
count if days_year_casual_labor_ganyu == 0

foreach i in hours_year_paid_job hours_year_firewood_collect days_year_casual_labor_ganyu {
	summ `i', det
	scalar p99_`i' = r(p99)
	replace `i' = . if `i' > p99_`i'
	g ln`i' = ln(`i')
	replace ln`i' = 0 if ln`i' ==.
}

summ hours_year_paid_job days_year_casual_labor_ganyu hours_year_firewood_collect, det

g lnp_transport = ln(transport_pindex)

lab var lnhours_year_paid_job "log(Market Work)"
lab var lndays_year_casual_labor_ganyu "log(Casual Work)"
lab var lnhours_year_firewood_collect "log(Hrs. Firewood)"
lab var age "Age"
lab var transport_pindex "Transport Price Index"
lab var lnpk "log(Kerosene price)"
lab var lnpc "log(Charcoal price)"
lab var lnpf "log(Firewood price)"
lab var ln_hh_total_exp "log(Household Exp.)"
lab var own_solar_panel "Own solar panel"
lab var literacy_chichewa "Lit.(Chichewa)"
lab var literacy_english "Lit.(English)"
lab var lnhours_year_paid_job "log(Market L.)"
lab var lndays_year_casual_labor_ganyu "log(Casual L.)"

g season = 0
replace season = 1 if month == 6 | month == 7
replace season = 2 if month == 8 | month == 9
replace season = 3 if month > 9

g person_id = PID
destring person_id, replace

duplicates report PID year
duplicates report person_id year

xtset person_id year


*Charcoal prices*
xtreg lnpc lnpf i.year, fe vce(robust)
predict fitted

g p_char_res = lnpc - fitted


*Main text in paper*

*Adult Labour Supply - Salaried & Casual Work*
est drop _all

*Women - Market Work*
xttobit lnhours_year_paid_job transport_pindex age i.education i.stratum i.season if adult_female == 1, vce(bootstrap) ll(0)
*eststo: margins, dydx(*) atmeans

xttobit lnhours_year_paid_job transport_pindex ln_hh_total_exp lnpf age i.education i.stratum i.season if adult_female == 1, vce(bootstrap) ll(0)
eststo: margins, dydx(*) atmeans

*Women - Casual Labour*
xttobit lndays_year_casual_labor_ganyu transport_pindex age i.education i.stratum i.season if adult_female == 1, vce(bootstrap) ll(0)
*eststo: margins, dydx(*) atmeans

xttobit lndays_year_casual_labor_ganyu transport_pindex age ln_hh_total_exp lnpf i.education i.stratum i.season if adult_female == 1, vce(bootstrap) ll(0)
eststo: margins, dydx(*) atmeans

*Men - Market Work*
xttobit lnhours_year_paid_job transport_pindex age i.education i.stratum i.season if adult_male == 1, vce(bootstrap) ll(0)
*eststo: margins, dydx(*) atmeans

xttobit lnhours_year_paid_job transport_pindex age ln_hh_total_exp lnpf i.education i.stratum i.season if adult_male == 1, vce(bootstrap) ll(0)
eststo: margins, dydx(*) atmeans

*Men - Casual Labour*
xttobit lndays_year_casual_labor_ganyu lnp_transport age i.education i.stratum i.season if adult_male == 1, vce(bootstrap) ll(0)
eststo: margins, dydx(*) atmeans

xttobit lndays_year_casual_labor_ganyu lnp_transport age ln_hh_total_exp lnpf i.education i.stratum i.season if adult_male == 1, vce(bootstrap) ll(0)
eststo: margins, dydx(*) atmeans

*Boys - Casual Labour*
xttobit lndays_year_casual_labor_ganyu lnp_transport age i.stratum i.season if adult == 0 & sex == 0, vce(bootstrap) ll(0)
*eststo: margins, dydx(*) atmeans

xttobit lndays_year_casual_labor_ganyu lnp_transport age ln_hh_total_exp lnpf i.stratum i.season if adult == 0 & sex == 0, vce(bootstrap) ll(0)
eststo: margins, dydx(*) atmeans

*Girls - Casual Labour*
xttobit lndays_year_casual_labor_ganyu lnp_transport age i.stratum i.season if adult == 0 & sex == 1, vce(bootstrap) ll(0)
*eststo: margins, dydx(*) atmeans

xttobit lndays_year_casual_labor_ganyu lnp_transport age ln_hh_total_exp lnpf i.stratum i.season if adult == 0 & sex == 1, vce(bootstrap) ll(0)
eststo: margins, dydx(*) atmeans

esttab est* using "reg_results\retobit_margeffect_person_fwood_collect.tex", replace scalars(chi2) b(%5.3f) se(%5.3f) star(* 0.10 ** 0.05 *** 0.01) par compress nobase drop(1.season 2.season 3.season 2.stratum 3.stratum 4.stratum 5.stratum 6.stratum) label note(Bootstrap standard errors in parentheses.)


*Table in paper*
est drop _all

xttobit lnhours_year_paid_job transport_pindex ln_hh_total_exp lnpf age i.education i.stratum i.season if adult_female == 1, vce(bootstrap) ll(0)
eststo: margins, dydx(*) atmeans

xttobit lndays_year_casual_labor_ganyu transport_pindex age ln_hh_total_exp lnpf i.education i.stratum i.season if adult_female == 1, vce(bootstrap) ll(0)
eststo: margins, dydx(*) atmeans

xttobit lnhours_year_paid_job transport_pindex age ln_hh_total_exp lnpf i.education i.stratum i.season if adult_male == 1, vce(bootstrap) ll(0)
eststo: margins, dydx(*) atmeans

xttobit lndays_year_casual_labor_ganyu transport_pindex age ln_hh_total_exp lnpf i.education i.stratum i.season if adult_male == 1, vce(bootstrap) ll(0)
eststo: margins, dydx(*) atmeans

xttobit lndays_year_casual_labor_ganyu transport_pindex age ln_hh_total_exp lnpf i.stratum i.season if adult == 0 & sex == 0, vce(bootstrap) ll(0)
eststo: margins, dydx(*) atmeans

xttobit lndays_year_casual_labor_ganyu transport_pindex age ln_hh_total_exp lnpf i.stratum i.season if adult == 0 & sex == 1, vce(bootstrap) ll(0)
eststo: margins, dydx(*) atmeans

esttab est* using "reg_results\retobit_margeffect_person_labour.tex", replace scalars(chi2) b(%5.3f) se(%5.3f) star(* 0.10 ** 0.05 *** 0.01) par compress nobase drop(1.season 2.season 3.season 2.stratum 3.stratum 4.stratum 5.stratum 6.stratum) label note(Bootstrap standard errors in parentheses.)


*Effects on Firewood Collection Time*
est drop _all

xttobit lnhours_year_firewood_collect transport_pindex age i.education i.stratum i.season if adult_female == 1, vce(bootstrap) ll(0) // Women
eststo: margins, dydx(*) atmeans

xttobit lnhours_year_firewood_collect transport_pindex ln_hh_total_exp lnpf age i.education i.stratum i.season if adult_female == 1, vce(bootstrap) ll(0) // Women
eststo: margins, dydx(*) atmeans

xttobit lnhours_year_firewood_collect transport_pindex age i.education i.stratum i.season if adult_male == 1, vce(bootstrap) ll(0) // Men
eststo: margins, dydx(*) atmeans

xttobit lnhours_year_firewood_collect transport_pindex ln_hh_total_exp lnpf age i.education i.stratum i.season if adult_male == 1, vce(bootstrap) ll(0) // Men
eststo: margins, dydx(*) atmeans

xttobit lnhours_year_firewood_collect transport_pindex age i.stratum i.season if adult == 0, vce(bootstrap) ll(0) // Children
eststo: margins, dydx(*) atmeans

xttobit lnhours_year_firewood_collect transport_pindex ln_hh_total_exp lnpf age i.stratum i.season if adult == 0, vce(bootstrap) ll(0) // Children
eststo: margins, dydx(*) atmeans

esttab est* using "reg_results\retobit_margeffect_person_fwood_collect.tex", replace scalars(chi2) b(%5.3f) se(%5.3f) star(* 0.10 ** 0.05 *** 0.01) par compress nobase drop(1.season 2.season 3.season 2.stratum 3.stratum 4.stratum 5.stratum 6.stratum) label note(Bootstrap standard errors in parentheses.)


*Children's literacy - Extensive Margin*
est drop _all

*Children's education*
est drop _all

*Boys*
xtprobit literacy_chichewa lnp_transport ln_hh_total_exp lnpf age i.stratum i.season if adult == 0 & sex == 0, re vce(robust)
eststo: margins, dydx(*) atmeans

xtprobit literacy_english lnp_transport ln_hh_total_exp lnpf age i.stratum i.season if adult == 0 & sex == 0, re vce(robust)
eststo: margins, dydx(*) atmeans

*Girls*
xtprobit literacy_chichewa lnp_transport ln_hh_total_exp lnpf age i.stratum i.season if adult == 0 & sex == 1, re vce(robust)
eststo: margins, dydx(*) atmeans

xtprobit literacy_english lnp_transport ln_hh_total_exp lnpf age i.stratum i.season if adult == 0 & sex == 1, re vce(robust)
eststo: margins, dydx(*) atmeans

esttab est* using "reg_results\retobit_margeffect_children_literacy.tex", replace scalars(chi2) b(%5.3f) se(%5.3f) star(* 0.10 ** 0.05 *** 0.01) par compress nobase drop(1.season 2.season 3.season 2.stratum 3.stratum 4.stratum 5.stratum 6.stratum) label note(Robust standard errors in parentheses.)



*Results with kerosene prices, instead of transport prices*
*Effects on Firewood Collection Time*
est drop _all

xttobit lnhours_year_firewood_collect lnpk age i.education i.stratum i.season if adult_female == 1, vce(bootstrap) ll(0) // Women
*eststo: margins, dydx(*) atmeans

xttobit lnhours_year_firewood_collect lnpk ln_hh_total_exp lnpf age i.education i.stratum i.season if adult_female == 1, vce(bootstrap) ll(0) // Women
*eststo: margins, dydx(*) atmeans

xttobit lnhours_year_firewood_collect lnpk age i.education i.stratum i.season if adult_male == 1, vce(bootstrap) ll(0) // Men
*eststo: margins, dydx(*) atmeans

xttobit lnhours_year_firewood_collect lnpk ln_hh_total_exp lnpf age i.education i.stratum i.season if adult_male == 1, vce(bootstrap) ll(0) // Men
*eststo: margins, dydx(*) atmeans

xttobit lnhours_year_firewood_collect lnpk age i.stratum i.season if adult == 0, vce(bootstrap) ll(0) // Children
*eststo: margins, dydx(*) atmeans

xttobit lnhours_year_firewood_collect lnpk ln_hh_total_exp lnpf age i.stratum i.season if adult == 0, vce(bootstrap) ll(0) // Children
eststo: margins, dydx(*) atmeans



*Adult Labour Supply - Salaried & Casual Work*
est drop _all

*Women - Market Work*
xttobit lnhours_year_paid_job lnpk ln_hh_total_exp lnpf age i.education i.stratum i.season if adult_female == 1, vce(bootstrap) ll(0)
eststo: margins, dydx(*) atmeans

*Women - Casual Labour*
xttobit lndays_year_casual_labor_ganyu lnpk ln_hh_total_exp lnpf age i.education i.stratum i.season if adult_female == 1, vce(bootstrap) ll(0)
eststo: margins, dydx(*) atmeans

*Men - Market Work*
xttobit lnhours_year_paid_job lnpk ln_hh_total_exp lnpf age i.education i.stratum i.season if adult_male == 1, vce(bootstrap) ll(0)
eststo: margins, dydx(*) atmeans

*Men - Casual Labour*
xttobit lndays_year_casual_labor_ganyu lnpk ln_hh_total_exp lnpf age i.education i.stratum i.season if adult_male == 1, vce(bootstrap) ll(0)
eststo: margins, dydx(*) atmeans

*Boys - Casual Labour*
xttobit lndays_year_casual_labor_ganyu lnpk ln_hh_total_exp lnpf age i.stratum i.season if adult == 0 & sex == 0, vce(bootstrap) ll(0)
eststo: margins, dydx(*) atmeans

*Girls - Casual Labour*
xttobit lndays_year_casual_labor_ganyu lnpk ln_hh_total_exp lnpf age i.stratum i.season if adult == 0 & sex == 1, vce(bootstrap) ll(0)
eststo: margins, dydx(*) atmeans

esttab est* using "reg_results\retobit_margeffect_person_labour_ker.tex", replace scalars(chi2) b(%5.3f) se(%5.3f) star(* 0.10 ** 0.05 *** 0.01) par compress nobase drop(1.season 2.season 3.season 2.stratum 3.stratum 4.stratum 5.stratum 6.stratum) label note(Bootstrap standard errors in parentheses.)


*Children's literacy - Extensive Margin*
est drop _all

*Boys*
xtprobit literacy_chichewa lnpk ln_hh_total_exp lnpf age i.stratum i.season if adult == 0 & sex == 0, re vce(robust)
eststo: margins, dydx(*) atmeans

xtprobit literacy_english lnpk ln_hh_total_exp lnpf age i.stratum i.season if adult == 0 & sex == 0, re vce(robust)
eststo: margins, dydx(*) atmeans

*Girls*
xtprobit literacy_chichewa lnpk ln_hh_total_exp lnpf age i.stratum i.season if adult == 0 & sex == 1, re vce(robust)
eststo: margins, dydx(*) atmeans

xtprobit literacy_english lnpk ln_hh_total_exp lnpf age i.stratum i.season if adult == 0 & sex == 1, re vce(robust)
eststo: margins, dydx(*) atmeans

esttab est* using "reg_results\retobit_margeffect_children_literacy_ker.tex", replace scalars(chi2) b(%5.3f) se(%5.3f) star(* 0.10 ** 0.05 *** 0.01) par compress nobase drop(1.season 2.season 3.season 2.stratum 3.stratum 4.stratum 5.stratum 6.stratum) label note(Robust standard errors in parentheses.)


*Testing the effect of charcoal prices*

*Adult Female*
xttobit lnhours_year_firewood_collect p_char_res lnpf ln_hh_total_exp age i.education i.stratum i.year if adult_female == 1, vce(bootstrap) ll(0)

xttobit lnhours_year_paid_job p_char_res lnpf ln_hh_total_exp age i.education i.stratum i.season if adult_female == 1, vce(bootstrap) ll(0)

xttobit lndays_year_casual_labor_ganyu p_char_res lnpf ln_hh_total_exp age i.education i.stratum i.season if adult_female == 1, vce(bootstrap) ll(0)

*Adult Male*
xttobit lnhours_year_firewood_collect p_char_res lnpf ln_hh_total_exp age i.education i.stratum i.year if adult_male == 1, vce(bootstrap) ll(0)

xttobit lnhours_year_paid_job p_char_res lnpf ln_hh_total_exp age i.education i.stratum i.season if adult_male == 1, vce(bootstrap) ll(0)

xttobit lndays_year_casual_labor_ganyu p_char_res lnpf ln_hh_total_exp age i.education i.stratum i.season if adult_male == 1, vce(bootstrap) ll(0)

*Children*
xttobit lnhours_year_firewood_collect p_char_res lnpf ln_hh_total_exp age i.education i.stratum i.year if adult == 0, vce(bootstrap) ll(0)

xttobit lndays_year_casual_labor_ganyu p_char_res lnpf ln_hh_total_exp age i.education i.stratum i.season if adult == 0, vce(bootstrap) ll(0)




