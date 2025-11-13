# Deployment Guide

This guide covers deploying both the public and internal Foodbook apps to Posit Connect.

## Overview

The Foodbook project consists of **two separate applications**:

1. **Public App** (`app-public.R`) - For PT users and external partners
   - Analysis workflow (manual entry, CSV upload, custom exposures)
   - Uses Open Canada public data exclusively
   - Fully bilingual (EN/FR)
   - No sensitive data

2. **Internal App** (`app-internal.R`) - For PHAC internal use
   - CEDARS Excel upload workflow
   - Uses Open Canada data + optional legacy microdata
   - Fully bilingual (EN/FR)
   - Internal access only

Both apps share the same backend (`src/foodbook_backend.R`) and support infrastructure.

## Prerequisites

### Required Files
Both apps require:
- `data/open-canada/foodbook-1/` - Foodbook 1 public microdata (EN + FR)
- `data/open-canada/foodbook-2/` - Foodbook 2 public microdata (EN + FR)
- `src/foodbook_backend.R` - Shared backend logic
- `src/i18n_helper.R` - Internationalization helpers
- `src/modules/` - Shiny modules
- `translations/translation.json` - UI translations (EN/FR)

Internal app additionally supports:
- `upgrade-context/` - Optional legacy microdata for expanded exposure coverage

### R Version
- **Recommended:** R 4.5.1 (as specified in manifests)
- **Minimum:** R 4.4.x

### Required Packages
See `manifest-public.json` and `manifest-internal.json` for full dependency lists.

**Key dependencies:**
- `shiny`, `bslib`, `thematic`
- `dplyr`, `purrr`, `tidyr`, `stringr`
- `data.table` (for fast CSV loading)
- `DT`, `ggplot2`
- `shinyjs`, `shinycssloaders`
- **`shiny.i18n`** (for bilingual support - NEW)
- `readxl` (internal app only - for Excel upload)
- `haven` (internal app optional - for legacy .dta files)

## Deployment Methods

### Method 1: Git-Backed Deployment (Recommended)

This method allows automatic updates when you push to the repository.

#### 1. Prepare Repository

```bash
# Ensure Open Canada data is committed
git add data/open-canada/foodbook-1/
git add data/open-canada/foodbook-2/

# For internal app, ensure upgrade-context is .gitignored (sensitive data)
echo "upgrade-context/" >> .gitignore

# Commit all files
git add .
git commit -m "Add Open Canada data and bilingual support"
git push origin main
```

#### 2. Configure Posit Connect

**For Public App:**
1. Log into Posit Connect
2. Click "New Content" → "Import from Git"
3. Select your repository
4. **Content Type:** Shiny Application
5. **App File:** `app-public.R`
6. **Branch:** `main`
7. **Manifest File:** `manifest-public.json`
8. Click "Deploy"

**For Internal App:**
1. Repeat steps above but use:
   - **App File:** `app-internal.R`
   - **Manifest File:** `manifest-internal.json`
2. Set access control to "Specific users/groups" (PHAC internal only)

#### 3. Configure Data Access (Internal App Only)

If using legacy microdata:
1. SSH into Posit Connect server
2. Create `upgrade-context/` directory in app's data folder
3. Copy `.dta` and `.do` files:
   ```bash
   cp foodbook.dta /opt/rstudio-connect/data/[app-id]/upgrade-context/
   cp foodbook2v2.dta /opt/rstudio-connect/data/[app-id]/upgrade-context/
   cp *.do /opt/rstudio-connect/data/[app-id]/upgrade-context/
   ```
4. Restart the app

### Method 2: Push-Button Deployment

Use `rsconnect` package for manual deployment.

#### 1. Setup

```r
# Install rsconnect
install.packages("rsconnect")
library(rsconnect)

# Add Connect server
rsconnect::addConnectServer(
  url = "https://your-connect-server.com",
  name = "prod"
)

# Authenticate (opens browser)
rsconnect::connectUser(server = "prod")
```

#### 2. Deploy Public App

```r
rsconnect::deployApp(
  appDir = ".",
  appFiles = c(
    "app-public.R",
    "src/foodbook_backend.R",
    "src/i18n_helper.R",
    "src/modules/exposure_module.R",
    "src/modules/language_selector_module.R",
    "translations/translation.json",
    "data/open-canada/"  # Include all Open Canada data
  ),
  appPrimaryDoc = "app-public.R",
  appName = "foodbook-public",
  server = "prod",
  account = "your-account",
  forceUpdate = TRUE
)
```

#### 3. Deploy Internal App

```r
rsconnect::deployApp(
  appDir = ".",
  appFiles = c(
    "app-internal.R",
    "src/foodbook_backend.R",
    "src/i18n_helper.R",
    "src/modules/language_selector_module.R",
    "translations/translation.json",
    "data/open-canada/",
    "upgrade-context/"  # Optional - only if using legacy data
  ),
  appPrimaryDoc = "app-internal.R",
  appName = "foodbook-internal",
  server = "prod",
  account = "your-account",
  forceUpdate = TRUE
)
```

## Post-Deployment Configuration

### 1. Set Access Controls

**Public App:**
- **Viewers:** All authenticated users or public (depending on policy)
- **Collaborators:** PT epidemiologists

**Internal App:**
- **Viewers:** PHAC FEB/OMD members only
- **Collaborators:** App administrators

### 2. Configure Runtime Settings

In Posit Connect UI:
1. Go to app settings → Runtime
2. **Max Processes:** 3-5 (depending on expected load)
3. **Min Processes:** 1
4. **Max Connections per Process:** 50
5. **Idle Timeout:** 15 minutes
6. **Init Timeout:** 60 seconds (apps load Open Canada data at startup)

### 3. Set Custom URL (Optional)

Configure vanity URLs:
- Public: `https://connect.phac/foodbook-public`
- Internal: `https://connect.phac/foodbook-cedars`

### 4. Email Notifications

Enable email notifications for:
- Deployment failures
- Runtime errors
- Scheduled report delivery (if applicable)

## Testing Deployment

### 1. Verify Data Loading

Check app logs for startup messages:
```
Loaded Foodbook 2 microdata from Open Canada (21744 respondents)
Loaded Foodbook 1 microdata from Open Canada (10892 respondents)
```

### 2. Test Bilingual Functionality

1. Open app in browser
2. Click language selector (EN ↔ FR)
3. Verify:
   - UI text translates
   - PT names translate
   - Exposure labels translate
   - Table headers translate

### 3. Test Core Features

**Public App:**
- Manual entry with multiple exposures
- CSV upload
- Custom exposures with custom references
- URL bookmarking (`?lang=fr`)
- Results export (CSV, copy, print)

**Internal App:**
- CEDARS Excel upload
- Sheet auto-detection
- PT/Age/Month filtering
- Results export with descriptive filenames

### 4. Performance Testing

- Upload large CEDARS file (1000+ cases) - should complete < 30 seconds
- Switch languages - should be instant
- Change filters - should recompute references < 2 seconds

## Troubleshooting

### "Foodbook 2 data not found"

**Cause:** Open Canada data files missing or wrong path

**Fix:**
```bash
# Verify files exist
ls data/open-canada/foodbook-2/*.csv

# Check file permissions
chmod 644 data/open-canada/foodbook-2/*.csv
```

### "Package 'shiny.i18n' not found"

**Cause:** Manifest not including shiny.i18n dependency

**Fix:**
```r
# Update manifest
rsconnect::writeManifest(
  appDir = ".",
  appPrimaryDoc = "app-public.R",
  appFiles = c(...)
)
```

### Language switching not working

**Cause:** Translator not initialized or translation.json missing

**Fix:**
1. Verify `translations/translation.json` is deployed
2. Check app logs for initialization errors
3. Restart app

### CEDARS upload fails

**Cause:** Sheet detection failing or column names mismatch

**Fix:**
1. Verify Excel file is valid (.xlsx format)
2. Check logs for specific error message
3. Ensure required columns exist: `NationalID`, `ExposureCode`, `HasExposureOccurred`

### Slow startup (> 60 seconds)

**Cause:** Loading large microdata files

**Fix:**
1. Increase Init Timeout in Connect settings to 90-120 seconds
2. Consider preloading/caching data (advanced)
3. Verify CSV files are not corrupted

## Updating Apps

### Method 1: Git-Backed (Automatic)

```bash
# Make changes
git add .
git commit -m "Update translation strings"
git push origin main

# Posit Connect automatically redeploys
```

### Method 2: Manual Push

```r
# Redeploy with forceUpdate
rsconnect::deployApp(
  appDir = ".",
  appName = "foodbook-public",
  server = "prod",
  forceUpdate = TRUE
)
```

## Monitoring

### Health Checks

Monitor these metrics in Posit Connect:
- **CPU Usage:** Should stay < 50% under normal load
- **Memory Usage:** ~500 MB per process (microdata loaded)
- **Request Duration:** < 5 seconds for most operations
- **Error Rate:** < 1%

### Log Monitoring

Key log messages to watch for:
- ✅ "Loaded Foodbook 2 microdata from Open Canada"
- ⚠️ "Foodbook 2 data not found, trying Foodbook 1..."
- ❌ "No microdata available"

### User Analytics

Track:
- Daily active users
- Language preference (EN vs FR)
- Most analyzed exposures
- Average session duration

## Backup & Recovery

### Backup Strategy

**Code & Configuration:**
- Git repository (primary backup)
- Posit Connect bundles (automatic)

**Data:**
- Open Canada data (public, can re-download)
- Legacy microdata (backup separately if using)

### Recovery Procedure

1. **App Failure:**
   - Check logs in Posit Connect
   - Roll back to previous bundle if needed
   - Contact administrator

2. **Data Corruption:**
   - Re-download Open Canada data from source
   - Verify checksums
   - Redeploy app

3. **Complete Loss:**
   - Clone repository
   - Download Open Canada data
   - Deploy fresh using Method 1 or 2

## Security Considerations

### Public App
- ✅ Uses only public Open Canada data
- ✅ No authentication required (or SSO if preferred)
- ✅ No sensitive data exposure
- ✅ Safe for external PT users

### Internal App
- ⚠️ CEDARS data uploaded by users (transient, not stored)
- ⚠️ Legacy microdata if using (keep restricted)
- ✅ Authentication required
- ✅ Access logging enabled

### Best Practices
1. **Never commit sensitive data** to Git
2. **Use `.gitignore`** for `upgrade-context/`
3. **Set proper file permissions** (644 for data, 755 for directories)
4. **Enable audit logging** in Posit Connect
5. **Regular security reviews** of access controls

## Support

### Documentation
- `README.md` - User guide
- `CLAUDE.md` - Developer guide
- `AGENTS.md` - Quick reference

### Contacts
- **App Developer:** [Your Name]
- **Posit Connect Admin:** [Admin Name]
- **Data Source:** Open Canada Portal

### Useful Links
- [Posit Connect Documentation](https://docs.posit.co/connect/)
- [Open Canada Foodbook Data](https://open.canada.ca/)
- [Shiny.i18n Documentation](https://github.com/Appsilon/shiny.i18n)

---

**Last Updated:** 2025-01-10
**Version:** 2.0 (Bilingual + Open Canada Release)
