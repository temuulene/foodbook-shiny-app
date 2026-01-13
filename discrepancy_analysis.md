# Foodbook Data Discrepancy Analysis

**Date:** 2026-01-08
**Purpose:** Analyze compatibility between the Excel Toolkit Exposure List and Foodbook 2.0 Microdata.

## Executive Summary

A systematic comparison of the **Excel Toolkit** exposure list against the **Foodbook 2.0 (stata) Microdata** reveals significant discrepancies that impact the functionality of the Shiny App (specifically Age Group and Month filtering).

*   **Total Exposures in Toolkit:** 379
*   **Matches in Microdata:** 247 (65%)
*   **Missing from Microdata:** 132 (35%)

**Key Finding:** The majority of the missing variables are legacy items from **Foodbook 1.0 (2014-2015)** which are included in the Toolkit for completeness but **were not collected** or are named differently in the Foodbook 2.0 (2023-2024) survey.

## Implications for Filtering

The Shiny App relies on the **Microdata** to calculate dynamic reference values for specific **Age Groups** and **Months**.

*   **For the 247 Matched Exposures:** Full functionality is available (Age/Month filtering works).
*   **For the 132 Missing Exposures:** 
    *   **Age/Month filtering is NOT possible.**
    *   The App must fallback to the static "All Ages / All Months" reference text from the Toolkit.
    *   Users attempting to filter these items will see data gaps (N/A) unless the fallback logic is applied.

## Detailed Analysis of Mismatches

The 132 missing variables fall into three primary categories:

### 1. Foodbook 1.0 Legacy Items (Marked with *)
The Excel Toolkit explicitly marks these with an asterisk (*). They represent foods that were surveyed in 2014 but dropped or changed in 2024.
**Examples:**
*   `anytom` (Any tomatoes*)
*   `cabbage` (Cabbage*)
*   `peas` (Peas*)
*   `anycarrot` (Any carrots*)
*   `freshherb` (Any fresh herbs)
*   `anyfrozenfruit` (Any frozen fruit*)

**Action for OMD:** Confirm if proxies exist in Foodbook 2.0 (e.g., aggregating `tomato_raw` + `tomato_cooked` to replace `anytom`) or if these should remain static/legacy only.

### 2. Missing Categories/aggregates
Some high-level categories present in the Toolkit do not appear to have direct distinct variables in the Microdata.
**Examples:**
*   `otheronions` (Other onions)
*   `otherveg` (Other vegetables)
*   `fruitjuice` (Unpasteurized fruit juice*) - *Marked FB1*
*   `otherfruit` (Other fruit)

### 3. Potential Naming Mismatches
A subset of variables might exist in the Microdata under different names. 
**Examples (Requiring Investigation):**
*   `bananas` (Toolkit) vs `banana` (Microdata?)
*   `shrimp` (Toolkit) vs `shellfish` subtypes?
*   `groundbeef` (Toolkit seems to split this into Specifics, but Toolkit has `anygroundbeef` as FB1).

## Full List of Missing Variables (Top 50)

| Variable ID | Label (Toolkit) | FB1 Only? | 
|:---|:---|:---|
| `anytom` | Any tomatoes* | TRUE |
| `bananas` | Bananas | FALSE |
| `cabbage` | Cabbage (includes coleslaw) | FALSE |
| `pearpod` | Peas (shelled or in pods)* | TRUE |
| `anycarrot` | Any carrots* | TRUE |
| `freshherb` | Any fresh herbs | FALSE |
| `tarragon` | Fresh tarragon* | TRUE |
| `anyspice` | Any spices* | TRUE |
| `prepsalad` | Any store-bought prepared salads?* | TRUE |
| `pastasalad` | Pasta salad (store-bought prepared)* | TRUE |
| `driedmango` | Dried mango | FALSE |
| `anyfrozenfruit` | Any frozen fruit* | TRUE |
| `frozenfruit` | Frozen fruit (not including berries)* | TRUE |
| `fruitjuice` | Unpasteurized fruit juice* | TRUE |
| `peanuts` | Peanuts (not including peanut butter) | FALSE |
| `almonds` | Almonds (excluding almond butter) | FALSE |
| `hazelnuts` | Hazelnuts (Filberts) | FALSE |
| `othernutbut` | Other nut paste, butter or spread* | TRUE |
| `anyseeds` | Any seeds | FALSE |
| `sesameprod` | Tahini, halva or other products made from sesame seeds* | TRUE |
| `anygroundbeef` | Any other ground beef* | TRUE |
| `anyhamburg` | Any hamburgers | FALSE |
| `frozpatty` | Store-bought frozen beef patties | FALSE |
| `homeburg` | Home-made hamburgers* | TRUE |
| `restburg` | Hamburgers from a restaurant or fast food establishment* | TRUE |
| `chickenrest` | Chicken from a restaurant or fast food establishment* | TRUE |
| `turkeysaus` | Turkey sausage | FALSE |
| `delineat` | Any deli-meat/cold cuts | FALSE |
| `chickendeli` | Chicken deli-meat | FALSE |
| `turkeydeli` | Turkey deli-meat | FALSE |
| `hamdeli` | Ham deli-meat | FALSE |
| `beefdeli` | Beef deli-meat | FALSE |
| `rawmeatorgans` | Any organ meats | FALSE |
| `kielbasa` | Kielbasa* | TRUE |
| `shawarma` | Shawarma or donair | FALSE |
| `shellfish` | Any shellfish | FALSE |
| `eggs` | Any eggs | FALSE |
| `rawmilk` | Unpasteurized dairy milk (not including cheese) | FALSE |
| `feta` | Feta | FALSE |
| `goatcheese` | Cheese made from goats milk | FALSE |
| `cheddar` | Cheddar* | TRUE |
| `mozz` | Mozzarella* | TRUE |
| `parm` | Parmesan* | TRUE |
| `procheese` | Processed cheese* | TRUE |
| `bvcheese` | Blue-veined cheese* | TRUE |
| `fcheese` | Cottage, ricotta or other fresh cheese* | TRUE |
| `gscheese` | Goat/sheep milk cheese* | TRUE |
| `flour` | Any wheat flour | FALSE |
| `hummus` | Hummus (excluding home-made) | FALSE |
| `anybabyformula` | Any baby formula* | TRUE |

## Recommendation

To resolve these differences, we recommend:
1.  **Accept the Hybrid Model**: Continue using the "Hybrid" approach implemented in the App (Microdata for matches, Static Toolkit Values for mismatches).
2.  **Disable Filters for Mismatches**: UI should meaningfully visually indicate (or disable) Age/Month filters when a "Toolkit Only" exposure is selected.
3.  **Review Mismatches**: Review the list of FALSE (Non-FB1) mismatches (e.g., `bananas`, `eggs`, `shellfish`) to see if they can be mapped to existing FB2 microdata columns with slightly different names.
