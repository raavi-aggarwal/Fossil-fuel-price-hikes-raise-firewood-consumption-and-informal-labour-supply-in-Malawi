
use "dhs_cookfuel.dta", clear

merge 1:1 hhid_temp using "dhs_women_power.dta"
drop _m

merge m:1 district month year using "energy_prices_dist_merge_dhs.dta"
drop _m

duplicates report hhid_temp

tab cooking_fuel
tab cooking_fuel, nolabel

g elec_fuel = 0
replace elec_fuel = 1 if cooking_fuel == 1

g charcoal_fuel = 0
replace charcoal_fuel = 1 if cooking_fuel == 2

g wood_fuel = 0
replace wood_fuel = 1 if cooking_fuel == 3

foreach i in f c k {
	g lnp`i' = ln(p_`i')
}

ihstrans wealth_index_score

lab var wood_fuel "Firewood Use"
lab var lnpk "log(Kerosene Price)"
lab var lnpf "log(Firewood Price)"
lab var parity "Decision-making Parity"
lab var women_power "Women empowerment"
lab var pow4_wife_earnings "Women's control: Own Earnings"
lab var ihs_wealth_index_score "IHS(Wealth Score)"

tab rural_urban
tab rural_urban, nolabel

label define urb 0 "Rural" 1 "Urban Household"
label values rural_urban urb

tab paraffin_lamp_yesno
tab paraffin_lamp_yesno, nolabel

label define lamp 0 "No lamp" 1 "Owns Paraffin Lamp"
label values paraffin_lamp_yesno lamp

label define pow 0 "No control" 1 "Partial/full control"
label values pow4_wife_earnings pow

*Regressions*

*Effects of bargaining power*
est drop _all

*Imputing missing prices*
egen m_lnpk = mean(lnpk)
replace lnpk = m_lnpk if lnpk ==.

*Sub-sample of HHs where women collect water (using water collection as a proxy for fuel collection)*
tab water_location
tab water_location, nolabel

*keep if water_location == 3

tab person_collecting_water
tab person_collecting_water, nolabel

*keep if person_collecting_water == 1

g female_water_collect = 0
replace female_water_collect = 1 if person_collecting_water == 1

*Regressions with parity*
probit wood_fuel lnpk parity i.region i.rural_urban i.paraffin_lamp_yesno, vce(robust)

probit wood_fuel lnpk parity ihs_wealth_index_score lnpf i.region i.rural_urban i.paraffin_lamp_yesno, vce(robust)

probit wood_fuel lnpk parity ihs_wealth_index_score lnpf i.region i.rural_urban i.paraffin_lamp_yesno, vce(robust)

margins, dydx(lnpk parity ihs_wealth_index_score lnpf) atmeans

margins, dydx(lnpk ihs_wealth_index_score) at(parity=1)
margins, dydx(lnpk ihs_wealth_index_score) at(parity=2)
margins, dydx(lnpk ihs_wealth_index_score) at(parity=3)
margins, dydx(lnpk ihs_wealth_index_score) at(parity=4)
margins, dydx(lnpk ihs_wealth_index_score) at(parity=5)

probit wood_fuel c.lnpk##c.parity##c.ihs_wealth_index_score lnpf i.region i.rural_urban i.paraffin_lamp_yesno, vce(robust)

margins, dydx(lnpk ihs_wealth_index_score lnpf) at(parity=5)

eststo: 
probit wood_fuel lnpk parity i.region i.rural_urban, vce(robust)
margins, dydx(lnpk)

eststo:
probit wood_fuel lnpk i.pow4_wife_earnings ihs_wealth_index_score i.region i.rural_urban, vce(robust)
margins, dydx(lnpk)

eststo: 
probit wood_fuel c.lnpk##i.pow4_wife_earnings i.region i.rural_urban, vce(robust)
margins, dydx(lnpk)

eststo: 
probit wood_fuel c.lnpk##c.women_power_gradation ihs_wealth_index_score i.region i.rural_urban, vce(robust)
margins, dydx(lnpk)

eststo: 
probit wood_fuel c.lnpk##c.women_power_gradation##c.ihs_wealth_index_score i.paraffin_lamp_yesno i.region i.rural_urban, vce(cluster region)
margins, dydx(lnpk)

esttab using "reg_results\probit_woodfuel_parity.tex", replace pr2 b(%5.3f) se(%5.3f) star(* 0.10 ** 0.05 *** 0.01) par compress nobase drop(2.region 3.region _cons) label note(Inclusion of regional controls. Standard errors (clustered by region) in parentheses.)


*Table (Appendix) - Extensive margin effects of kerosene price hikes*
est drop _all

eststo: probit wood_fuel lncrude_avg ihs_wealth_index_score i.region, vce(cluster region)
eststo: probit wood_fuel lngasoline ihs_wealth_index_score i.region, vce(cluster region)
eststo: probit wood_fuel lnpk i.region, vce(cluster region)
eststo: probit wood_fuel lnpk ihs_wealth_index_score i.paraffin_lamp_yesno i.region i.rural_urban, vce(cluster region)
eststo: ivprobit wood_fuel (lnpk = lncrude_avg) i.paraffin_lamp_yesno i.region i.rural_urban, vce(cluster region)
eststo: ivprobit wood_fuel (lnpk = lngasoline) i.paraffin_lamp_yesno i.region i.rural_urban, vce(cluster region)
eststo: probit charcoal_fuel lnpk ihs_wealth_index_score i.paraffin_lamp_yesno i.region i.rural_urban, vce(cluster region)
eststo: probit elec_fuel lnpk ihs_wealth_index_score i.paraffin_lamp_yesno i.region i.rural_urban, vce(cluster region)

esttab using "reg_results\probit_woodfuel_dhs.tex", replace pr2 scalars(chi2) b(%5.3f) se(%5.3f) star(* 0.10 ** 0.05 *** 0.01) par compress nobase drop(7.month 8.month 9.month 2.region 3.region _cons) label note(Inclusion of regional controls. Standard errors (clustered by region) in parentheses.)

