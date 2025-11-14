# Archive

This folder contains archived versions of files that are no longer in active use but are preserved for reference.

## app.R.legacy

The original combined Shiny application that contained both the public analysis workflow and the internal CEDARS upload workflow in a single file.

**Date Archived:** November 12, 2024

**Reason:** The application was split into two separate, purpose-built apps for better maintainability:
- `app-public/app.R` - Public-facing analysis workflow (manual entry, CSV upload, custom exposures)
- `app-internal/app.R` - Internal PHAC workflow (CEDARS Excel upload with auto-detection)

Both new apps share the same backend (`src/foodbook_backend.R`) and UI components (`src/modules/`) but have focused, streamlined interfaces for their specific use cases.

**Do NOT use this file for development.** All new features and bug fixes should be applied to `app-public/app.R` and/or `app-internal/app.R` in the project root.

The legacy file is preserved for:
- Historical reference
- Understanding the evolution of the codebase
- Emergency fallback if needed (though not recommended)
