
use "firewood_rf_panel_2010_13_19.dta", clear

merge m:1 month year reside using "energy_prices_malawi.dta"
keep if _m == 3
drop _m

g season = 0
replace season = 1 if month == 6 | month == 7
replace season = 2 if month == 8 | month == 9
replace season = 3 if month > 9

label define season 0 "Jan - May" 1 "Jun - Jul" 2 "Aug - Sep" 3 "Oct - Dec"
label values season season

*Firewood prices*
estpost tabstat p_dist_f if year == 2019, by(stratum) statistics(count mean sd min max) columns(statistics)

esttab . using "descriptives\stats\wood_prices_region.tex", cells("count(fmt(%9.0gc)) mean(fmt(%9.2gc)) sd(fmt(%9.2gc)) min(fmt(%9.2gc)) max(fmt(%9.2gc))") label replace

estpost tabstat p_dist_f if year == 2019, by(season) statistics(count mean sd min max)

esttab . using "descriptives\stats\wood_prices_season.tex", cells("count(fmt(%9.0gc)) mean(fmt(%9.2gc)) sd(fmt(%9.2gc)) min(fmt(%9.2gc)) max(fmt(%9.2gc))") label replace


*Firewood quantity*
count if firewood_value_annual == 0
drop if firewood_value_annual ==. // We exclude missing observations

g lnf_value = ln(firewood_value_annual)
replace lnf_value = 0 if lnf_value ==.

g lnf = lnf_value - pf
replace lnf = 0 if lnf ==.

tab reside
tab reside, nolabel
recode reside (2 = 0)

label define res 0 "Rural Household" 1 "Urban Household"
label values reside res

tab own_solar_panel
tab own_solar_panel, nolabel

label define solar 0 "No Solar Panel" 1 "Owns Solar Panel"
label values own_solar_panel solar

g lnp_transport = ln(transport_pindex)

lab var lnf "log(Firewood)"
lab var transport_pindex "Transport Price Index"
lab var pk "log(Kerosene price)"
lab var pc "log(Charcoal price)"
lab var pf "log(Firewood price)"
lab var ln_hh_total_exp "log(Household Exp.)"
lab var own_solar_panel "Own solar panel"

g season = 0
replace season = 1 if month == 6 | month == 7
replace season = 2 if month == 8 | month == 9
replace season = 3 if month > 9

*ID*
duplicates report HHID year
xtset HHID year


*Charcoal prices*
pwcorr pc pf transport_pindex, star(0.05)

xtreg pc transport_pindex i.year, fe vce(robust)

xtreg pc pf i.year, fe vce(robust)
predict fitted

g p_char_res = pc - fitted


*Fixed effects panel regressions*
pwcorr transport_pindex pk pc pf ln_hh_total_exp, star(0.05)

est drop _all

*Effects on firewood value*
eststo: xtreg lnf_value pk i.year, fe vce(cluster ea_id)
eststo: xtreg lnf_value pk i.season, fe vce(cluster ea_id)
eststo: xtreg lnf_value pk pf i.season, fe vce(cluster ea_id)

eststo: xtreg lnf_value transport_pindex i.year, fe vce(cluster ea_id)
eststo: xtreg lnf_value transport_pindex i.season, fe vce(cluster ea_id)
eststo: xtreg lnf_value transport_pindex pf i.season, fe vce(cluster ea_id)

esttab using "reg_results\fe_fwood_value.tex", replace r2 b(%5.3f) se(%5.3f) star(* 0.10 ** 0.05 *** 0.01) par compress nobase drop(1.season 2.season 3.season 2013.year 2019.year) label note(Standard errors (clustered by enumeration area) in parentheses.)


*Effects on firewood quantity*
est drop _all

eststo: xtreg lnf pk pf i.year, fe vce(cluster ea_id)
eststo: xtreg lnf pk pf ln_hh_total_exp i.own_solar_panel i.season, fe vce(cluster ea_id)
eststo: xtreg lnf pk pf ln_hh_total_exp i.own_solar_panel i.stratum i.season, fe vce(cluster ea_id)

eststo: xtreg lnf transport_pindex pf i.year, fe vce(cluster ea_id)
eststo: xtreg lnf transport_pindex pf ln_hh_total_exp i.own_solar_panel i.season, fe vce(cluster ea_id)
eststo: 
xtreg lnf transport_pindex pf ln_hh_total_exp i.own_solar_panel i.stratum i.season, fe vce(cluster ea_id)

esttab using "reg_results\fe_fwood_qty.tex", replace r2 b(%5.3f) se(%5.3f) star(* 0.10 ** 0.05 *** 0.01) par compress nobase drop(1.season 2.season 3.season 2.stratum 3.stratum 4.stratum 5.stratum 6.stratum 2013.year 2019.year) label note(Standard errors (clustered by enumeration area) in parentheses.)


*Effects of charcoal prices*
est drop _all

eststo: xtreg lnf p_char_res pf i.year, fe vce(cluster ea_id)
eststo: xtreg lnf p_char_res pf ln_hh_total_exp i.year, fe vce(cluster ea_id)
eststo: xtreg lnf p_char_res pf i.season, fe vce(cluster ea_id)
eststo: xtreg lnf p_char_res pf ln_hh_total_exp i.season, fe vce(cluster ea_id)

esttab using "reg_results\fe_fwood_qty_char_prices.tex", replace r2 b(%5.3f) se(%5.3f) star(* 0.10 ** 0.05 *** 0.01) par compress nobase drop(1.season 2.season 3.season 2013.year 2019.year) label note(Standard errors (clustered by enumeration area) in parentheses.)


*Extensive margin of firewood use - collection vs. purchase*
est drop _all

xtprobit whether_fwood_purchased pk, re vce(cluster ea_id)
eststo: margins, dydx(*) atmeans

xtprobit whether_fwood_purchased pk ln_hh_total_exp pf i.stratum i.season, re vce(cluster ea_id)
eststo: margins, dydx(*) atmeans

xtprobit whether_fwood_purchased transport_pindex, re vce(cluster ea_id)
eststo: margins, dydx(*) atmeans

xtprobit whether_fwood_purchased transport_pindex ln_hh_total_exp pf i.stratum i.season, re vce(cluster ea_id)
eststo: margins, dydx(*) atmeans

esttab using "reg_results\re_fwood_collect_purchase.tex", replace scalars(chi2) b(%5.3f) se(%5.3f) star(* 0.10 ** 0.05 *** 0.01) par compress nobase drop(1.season 2.season 3.season 2.stratum 3.stratum 4.stratum 5.stratum 6.stratum) label note(Standard errors (clustered by enumeration area) in parentheses.)

*Charcoal prices*
xtprobit whether_fwood_purchased p_char_res, re vce(cluster ea_id)
eststo: margins, dydx(*) atmeans

xtprobit whether_fwood_purchased p_char_res pf ln_hh_total_exp i.stratum i.season, re vce(cluster ea_id)
eststo: margins, dydx(*) atmeans


*Effects on kerosene quantity*
est drop _all

g lnk = ln(kerosene_expenses_annual) - pk

eststo: xtreg lnk pk pf i.year, fe vce(cluster ea_id)
eststo: xtreg lnk pk pf ln_hh_total_exp i.own_solar_panel i.year, fe vce(cluster ea_id)
eststo: xtreg lnk pk pf i.season, fe vce(cluster ea_id)
eststo: xtreg lnk pk pf ln_hh_total_exp i.own_solar_panel i.season, fe vce(cluster ea_id)

esttab using "reg_results\ker_qty.tex", replace scalars(r2) b(%5.3f) se(%5.3f) star(* 0.10 ** 0.05 *** 0.01) par compress nobase drop(1.season 2.season 3.season 2013.year 2019.year 1.own_solar_panel) label note(Standard errors (clustered by enumeration area) in parentheses.)


*Charcoal use*
g lnc = ln(charcoal_expenses_annual/p_c)

xtreg lnc pk i.season, fe vce(cluster ea_id)
xtreg lnc pk pc pf ln_hh_total_exp i.own_solar_panel i.season, fe vce(cluster ea_id)
xtreg lnc pk pc pf ln_hh_total_exp i.own_solar_panel i.stratum i.season, fe vce(cluster ea_id)

xtreg lnc transport_pindex i.season, fe vce(cluster ea_id)
xtreg lnc transport_pindex pc pf ln_hh_total_exp i.own_solar_panel i.season, fe vce(cluster ea_id)
xtreg lnc transport_pindex pc pf ln_hh_total_exp i.own_solar_panel i.stratum i.season, fe vce(cluster ea_id)
