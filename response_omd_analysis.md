# Comprehensive Response: Toolkit vs OMD Data Coverage Analysis

## Executive Summary

**Direct Answer:** Yes, based on the current "Merged OMD" dataset (`upgrade-context/foodbook.dta` + `upgrade-context/foodbook2v2.dta`), you will **only be able to dynamically filter by age group and month for the 54.1% of exposures** that are present in that merged file.

The low coverage arises because the **Toolkit** represents the **UNION** of all variables ever asked (Foodbook 1 + Foodbook 2.0), while the current **Merged OMD Microdata** preparation process effectively restricts the dataset to the **INTERSECTION** (or common core) of variables, explicitly dropping those that do not align or were renamed.

---

## 1. Why is the coverage only 54.1%?

Although both the Toolkit and the OMD microdata originate from the same surveys (Foodbook 1 and Foodbook 2.0), they are processed differently.

### A. The Toolkit (Union of Data)
The Excel Toolkit appears to be an aggregation of **all valid results** from both surveys. If a question was asked in Foodbook 1 but not Foodbook 2, it is still included in the Toolkit (likely with a smaller sample size basis, just n=10,942).

### B. The Merged OMD Dataset (Intersection/Core Data)
The discrepancy comes from how the microdata was prepared for the app. The analysis of `TOOLKIT_VS_OMD_ANALYSIS.md` revealed a critical step in the OMD processing script:

```stata
drop Q*
```

This command in the Stata script removes original question variables (e.g., `Q1`, `Q2`...) before merging. This single step causes the loss of approximately **46% of variables** which:
1.  **Existed only in Foodbook 1** (37 variables like `ham`, `bacon`).
2.  **Existed only in Foodbook 2** (59 variables).
3.  **Were renamed or derived differently** between the two versions (71 variables).

### C. Confirmed by Reports
*   **Foodbook 1 Report**: Sample size **10,942**.
*   **Foodbook 2 Report**: Sample size **21,744**.
    *   **Note:** 1,424 respondents (6.5%) were excluded from food modules because they travelled outside their province/territory in the last 7 days. Ideally, these rows should be filtered out or treated as missing in the microdata for exposure calculations.
*   **Merged**: Total **32,686**.

The 54.1% commonality means only ~197 variables were kept in the "common core" merge process. The 2014 and 2023 surveys asked different questions (e.g., FB2 added "meal kits", FB1 had different meat details), so the union of variables is much larger than the intersection.

---

## 2. Implications for the Shiny App

### Scenario A: Using the Merged Microdata (Current State)
If the app relies solely on the merged microdata to calculate reference values dynamically (e.g., "Show me reference values for Males aged 20-40 in July"):
*   **You can ONLY do this for the 54.1% (197) variables.**
*   For the missing 46%, the app will return "No Data" or throw an error because those columns simply do not exist in the merged dataframe.

### Scenario B: Using the Toolkit Data (Static)
The Toolkit data is static. It has the overall Canada-level averages. It **does not** contain the row-level data needed to filter by specific subgroups (Sample Size < Overall). 
*   **Limitation:** You cannot use the Toolkit to filter by Age/Month dynamically. You can only show the pre-calculated numbers.

---

## 3. Recommendation

To enable filtering for **100% of the exposures**, we cannot use the dataset as currently merged. We must adopt one of the following strategies:

### Strategy 1: "Loose" Merge (Recommended)
Re-process the microdata to **keep all variables**.
*   If `Bacon` exists in FB1 but not FB2, keep it.
*   For FB2 respondents, the `Bacon` column will be `NA` (Null).
*   **Result:** You can filter `Bacon` by Age/Month, but the sample size for that calculation will only be 10,942 (FB1 respondents only), not 32,686. This is statistically valid and allows full coverage.

### Strategy 2: Dual Source Loading
Modify the app to load `foodbook.dta` (FB1) and `foodbook2v2.dta` (FB2) separately.
*   Check which file contains the requested variable.
*   Calculate statistics from the relevant file(s).

### Conclusion
The low coverage is a **data processing artifact**, not a flaw in the source data itself. To fix it, we need to change how the merged dataset is constructed or how the app accesses the source files.
