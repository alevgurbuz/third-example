*****************************************************
** Project: Impact Evaluation for NET
** Author: Alev Gurbuz Cuneo
** Date Created: 07/22/2019
** Date 
****************************************

clear all
set more off

** Change the directory:
	
cd "C:\Users\WB502361\WBG\Julia Vaillant - Great Lakes SGBV IE DRC"

  global baseline = "11. Data analysis/baseline"
  global dta = "$baseline/dtafiles"
  global Tables = "$baseline/report/tables and graphs/tables"
  global Graphs = "$baseline/report/tables and graphs/graphs"

use "$dta\20190623_NET_Baseline_data_indicators_lb.dta"


** Hi Alev

** first create new variables:
gen a=.n
replace a=1 if b=1


** test
** new comment

