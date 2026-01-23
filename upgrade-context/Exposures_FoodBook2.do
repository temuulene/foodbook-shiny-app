/*
Code to compare case data from CEDARS to Foodbook
Author: Vanessa Morton
Version: 1
Last revised: September 25, 2024

Based on earlier code created by Shiona Glass-Kasstra

Need to have the following files in one folder; specified in Step 1A
Analysis Template.xlsx
foodbook data.do
foodbook.dta
foodbook2v2.dta
foodbook variable labeling.do
***Case data in excel***
*/

/******************************************************************************
INSTRUCTIONS
1. Make sure all files listed above are in one folder
--- to retrieve case data from CEDARS, go to Cases, filter by event ID, and click Export to Excel
--- save salmonella_cases.xlsx file in the folder with the other files needed for the analysis
2. Make sure AnalysisTemplate.xlsx does not contain data. If it does, delete the data in columns that 
do not contain code (Food Item, Count, Foodbook Reference) and re-save the file
3. If you have not run the FFA analysis before, install packages listed below (before STEP 1)
4. In STEP 1, edit A to link to the folder where all files needed for the analysis are saved
5. In STEP 1, check/edit pathogen listed in B
6. To analyze only a subset of your case data, edit this in STEP 3, as described
7. To only include Foodbook data from certain PTs, months or other variables, edit this in STEP 6, as described
8. Run this entire code. It may stall slightly at one point, but it will continue.
--- AnalysisTemplate.xlsx will now contain the data output, analysis of case data vs Foodbook data 
*******************************************************************************/

clear
program drop _all
set more off
**use the code below to install packages needed to run the code ***Only need to do once***
*ssc install tab_chi
*findit svmat2
*click on first link to install
*dm79 from http://www.stata.com/stb/stb56
/************************* STEP 1 - MANDATORY *********************************
			Set up variable for file import
			Can run off your desktop or Sharepoint
******************************************************************************/
	
*A: Pathway to your analysis folder or link to sharepoint
cd "C:\Users\MTOOBY\OneDrive - HC-SC PHAC-ASPC\Desktop\2025-046 FFA 2025-06-10"

*B: Pathogen options are: ecoli salmonella cyclospora hepa
global pathogen salmonella

*C: Excel file extracted from CEDARS
global invest "salmonella_cases.xlsx"

*D: Excel sheet containing exposure data
global sheet "case exposure answer"

*E: Excel sheet containing case demographics
global linelist "Salmonella Case"
/****************************** STEP 2 ****************************************
			Import case data and select exposures
******************************************************************************/

import excel using "$invest", sheet($sheet) first

*limit to confirmed cases
keep if Status=="Confirmed"

keep NationalID Exposurecode Hasexposureoccurred
rename Hasexposureoccurred Y 
rename NationalID natid

*Reshape data
reshape wide Y, i(natid) j(Exposurecode) string

*Rename exposure variable
renpfix Y

*Use varlist to give full list of exposure variables
unab varlist : `r(varlist)' 
display "`varlist'"

*Check that all variables are included 
*global exposures (firstexpousre)-(lastexposure)
if "$pathogen" == "salmonella" {
	global exposures `varlist'
}

save cases.dta, replace

*Merge exposure data with demographic data
clear
import excel using "$invest", sheet($linelist) first
	
keep natid casestatus ProvinceTerritory sexcase agecase earliestdate

merge 1:1 natid using "cases.dta"


/***************************** STEP 3 ***************************************
	If you need to look at a subset of your case data, this is where
	you do that! 
******************************************************************************/

*drop if ProvinceTerritory = "Alberta"
*drop if sexcase = "Male"
*drop if agecase
*drop if earliestdate

/***************************** STEP 4 ***************************************
	Create a summary of exposures for cases
******************************************************************************/

foreach var of varlist $exposures { 
	quietly: replace `var' = "1" if `var' == "y"
	quietly: replace `var' = "2" if `var' == "n"
	quietly: replace `var' = "3" if `var' == "p"
	quietly: replace `var' = "4" if `var' == "dk"
	destring `var', replace
}

tabm $exposures, matcell (x)

*Makes a list of all exposure variables
unab varlist : $exposures 
display "`varlist'"
local varcount = `:word count `varlist'' /*Counts the number of exposure variables*/
display `varcount'

*Name rows and columns
matrix rownames x = `varlist'
matrix colnames x = "Y" "N" "P" "DK"

svmat2 x,names(col) rnames(exposure)

drop natid casestatus ProvinceTerritory sexcase agecase earliestdate $exposures _merge 

save cases.dta, replace

/****************************** STEP 5 ****************************************
		Import foodbook data
******************************************************************************/

clear
do "foodbook data.do"

generate status = "control"
drop Q*  
drop N*
drop O*

*limit by month/age/gender
/******************************* STEP 6 ***************************************
	If you need to "slice and dice" the foodbook data, this is where
	it happens.  Turn the following lines "on" by deleting the stars
	and editing the provinces/months to drop:
	
	Provinces (PT)
	BC == 1		ON == 5		PE == 9		NU == 13
	AB == 2		QC == 6		NL == 10
	SK == 3		NB == 7		YK == 11
	MB == 4		NS == 8		NT == 12
	
	Gender
	Male == 1 	Female == 2 Another gender == 3
	
	Month 
	January == 1
	February == 2 etc.
******************************************************************************/
*keep if PT == 1 | PT == 2 | PT == 5 | PT == 12
*keep if Month == 2 | Month == 3 | Month == 4 | Month == 5

drop EXPWEIGHT_CMA2 ID Month DEMO2 CONT_METHOD Durationofconnectioninminutes DEMO6B T_C Language COMP_TYPE PT Age_group Gender Sex DEMO8
drop Ethnicity Education Income PROXY Weight Exclude_from_food_safety Telephone_Sample_Food_Safety Travel_Out_Of_Prov weight_mail proj_weight_mail weight_non_traveller proj_weight_non_traveller

svyset _n [pweight=weight], vce(linearized) singleunit(missing)

global fexposures celery-animfdrodent

putexcel set "Foodbook.xlsx", open sheet("Foodbook") replace

putexcel A1= "exposure" B1 = "Foodbook"

local row = 2

foreach var of varlist $fexposures  {
	capture svy: prop `var'
			if _rc ==0{
			matrix B = e(b)
			matrix C = B[1,1]
			putexcel B`row' = matrix(C)
			putexcel A`row' = ("`var'")
			}
		if _rc !=0 {
			putexcel A`row' = ("`var'")
			putexcel B`row' = (.)
			}
local ++row
	}
	
putexcel close

clear

import excel "Foodbook.xlsx", sheet("Foodbook") firstrow

merge 1:1 exposure using "cases.dta"

replace Foodbook = Foodbook * 100
*drop if _merge !=3
drop if _merge == 1

gsort  -Y

do "foodbook variable labeling.do"

export excel label Y P N DK using "AnalysisTemplate.xlsx", sheetmodify cell(B5) keepcellfmt
export excel Foodbook using "AnalysisTemplate.xlsx", sheetmodify cell(L5) keepcellfmt
