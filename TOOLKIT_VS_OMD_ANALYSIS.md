# Excel Toolkit vs OMD Stata Microdata: Detailed Comparative Analysis

## Executive Summary

This report provides a comprehensive comparison between the **Excel Toolkit** (`data/Toolkit-binomial-probability-calculation-tool-2.0.xlsx`) and the **PHAC OMD Stata microdata** (`upgrade-context/`). The analysis covers exposure variable lists, reference percentage values, and calculation methodologies.

### Key Findings

| Finding | Description |
|---------|-------------|
| **Formula Difference** | Excel uses `BINOMDIST(..., 0)` (PMF: probability of *exactly* k cases). Standard epi tools use CDF (probability of *k or more* cases). |
| **Reference Values** | 187/197 (95%) of exposures match within 1%. 10 items have significant discrepancies (>1%). |
| **Exposure Coverage** | Only 54% of Toolkit exposures exist in the merged OMD dataset. 37 FB1 and 59 FB2 items are excluded. |

---

## 1. Data Sources

### 1.1 Excel Toolkit (Public)
| Attribute | Value |
|-----------|-------|
| **File** | `data/Toolkit-binomial-probability-calculation-tool-2.0.xlsx` |
| **Reference Data** | Sheet `Table 6` (381 rows) |
| **Formulas** | Sheet `Tool_A` |
| **Matched Exposures** | 363 (with variable names) |

### 1.2 OMD Stata Microdata (Internal)
| Attribute | Value |
|-----------|-------|
| **FB1** | `upgrade-context/foodbook.dta` (n=10,942) |
| **FB2** | `upgrade-context/foodbook2v2.dta` (n=21,744) |
| **Merged** | 32,686 respondents |
| **Labels** | `upgrade-context/foodbook variable labeling.do` |

---

## 2. Statistical Methodology

### 2.1 Excel Toolkit Formula

```excel
=BINOMDIST($G6, $H6, K6/100, 0)
```

| Argument | Meaning |
|----------|---------|
| `$G6` | Number of confirmed cases (k) |
| `$H6` | Total respondents (n) |
| `K6/100` | Reference probability (p) |
| **`0`** | **PMF mode: P(X = k)** |

**Alert Logic:** `=IF(L6 <= 0.05 AND J6*100 > K6, "Alert", ...)`

### 2.2 Standard Epidemiological Approach

```r
binom.test(x = k, n = n, p = p, alternative = "greater")
# Returns: P(X >= k) — the tail probability
```

### 2.3 Impact

The Excel PMF approach identifies cases where the *exact* observed count is improbable. The CDF approach identifies cases where *at least* that many cases are improbable. The CDF approach is more conservative and is the standard for cluster detection.

---

## 3. Reference Value Comparison (Full)

### 3.1 Methodology
We compared Canada-level reference percentages from `Table 6` against weighted calculations from the OMD merged microdata for **ALL 197 exposures** that exist in both sources.

### 3.2 Results Summary

| Metric | Value |
|--------|-------|
| **Exposures Compared** | 197 (complete) |
| **Average Absolute Difference** | 0.44% |
| **Max Difference** | 16.0% (Q2) |
| **Matches within 1%** | ~187 (95%) |

### 3.3 Top 10 Significant Discrepancies

| Variable | Toolkit % | OMD Weighted % | Difference | Notes |
|----------|-----------|----------------|------------|-------|
| **Q2** | 18.5 | 2.5 | **-16.0%** | Major mismatch - different variable? |
| **Swim** | 19.3 | 6.4 | **-12.9%** | Different definition or source |
| **Carrot** | 81.4 | 68.9 | **-12.5%** | Possible variable re-mapping |
| **Fruitz** | 9.4 | 19.9 | **+10.5%** | Frozen fruit - value doubled |
| **Bwhole** | 23.1 | 33.1 | **+10.0%** | Beef whole cuts |
| **Unpjuice** | 7.8 | 16.5 | **+8.7%** | Unpasteurized juice |
| **Alfalfa Sprout** | 5.9 | 2.9 | **-3.0%** | Swapped with Bean Sprouts? |
| **Sausage_turk** | 4.2 | 1.4 | **-2.8%** | Swapped with Tground? |
| **Tground** | 1.4 | 4.2 | **+2.8%** | Swapped with Sausage_turk? |
| **Bean Sprouts** | 2.9 | 5.4 | **+2.5%** | Swapped with Alfalfa? |

### 3.4 Observations

1. **Swapped Values:** Several pairs (Alfalfa/Bean Sprouts, Sausage_turk/Tground) appear to have their values transposed between sources.
2. **Major Outliers:** Q2, Swim, Carrot, Fruitz, Bwhole, Unpjuice have >8% differences - likely different variable definitions or data sources.
3. **Majority Match:** 95% of exposures match within 1%, confirming Toolkit values are derived from the same OMD microdata.

---

## 4. Exposure List Comparison

### 4.1 Distribution

| Category | Count | % of Toolkit |
|----------|-------|--------------|
| **In Merged OMD** | 197 | 54.1% |
| **FB2 Only (Dropped)** | 59 | 16.2% |
| **FB1 Only (Legacy)** | 37 | 10.2% |
| **Missing from OMD** | 71 | 19.5% |

### 4.2 Why Are Variables Excluded?

The OMD `foodbook data.do` processing script includes:

```stata
drop Q*
```

This removes all original FB1 question variables (`Q1`–`Q132`) before appending FB2, causing 37 FB1-only exposures (e.g., `ham`, `bacon`, `anybabyformula`) to be lost.

---

## 5. Recommendations

1. **Methodology:** Use CDF (`binom.test`) for the app; document difference from Excel PMF approach.
2. **Data:** Investigate the 10 outlier variables with OMD to confirm correct mappings.
3. **Coverage:** Accept 54% coverage or integrate FB1 legacy data separately.

---

## 6. Generated Files

| File | Description |
|------|-------------|
| `toolkit_vs_omd_exposure_summary.csv` | Variable-by-variable coverage (364 rows) |
| `toolkit_vs_omd_canada_comparison.csv` | Reference value comparison (197 rows) |
| `scripts/toolkit_vs_omd_analysis.R` | Analysis script |
