# Handover Guide: Foodbook Shiny Apps (PHAC-OMD)

This guide provides step-by-step instructions for getting the Foodbook applications up and running in a new environment. 

**Prerequisites:**
*   You should have R and RStudio installed.
*   You should have basic familiarity with Git (using the terminal or a GUI like GitHub Desktop).

---

## Part 1: Setting up the Code Repository (GitHub)

The first step is to create a home for the code on GitHub. This allows your team to collaborate and track changes.

### Step 1: Create a Blank Repository
1.  Log in to your organization's GitHub (e.g., github.com or your enterprise instance).
2.  Click the **+** icon in the top-right corner and select **New repository**.
3.  **Repository name**: `foodbook-shiny-app`
4.  **Description** (Optional): "Foodbook Shiny App for internal and public use."
5.  **Visibility**: Choose **Private** (recommended for initial setup) or **Internal** depending on your organization's policy.
6.  **Initialize this repository**: **Leave all checkboxes UNCHECKED**. We are importing existing code, so we need an empty repository.
7.  Click **Create repository**.
8.  **Copy the repository URL** (it will look like `https://github.com/phac-omd/foodbook-shiny-app.git`). You will need this in the next step.

### Step 2: Push the Code
Now, we will send the code from your local computer to the new GitHub repository.

1.  Open your terminal (or Git Bash) and navigate to the project folder.
2.  Run the following commands one by one:

    ```bash
    # 1. Initialize a new git repository in this folder
    git init

    # 2. Add all files to the staging area
    git add .

    # 3. Commit the files (save a snapshot)
    git commit -m "Initial commit for PHAC-OMD handover"

    # 4. Link your local folder to the GitHub repository you just created
    # IMPORTANT: Replace the URL below with YOUR repository URL from Step 1
    git remote add origin https://github.com/phac-omd/foodbook-shiny-app.git

    # 5. Rename the default branch to 'main'
    git branch -M main

    # 6. Push the code to GitHub
    git push -u origin main
    ```

---

## Part 2: Setting up Hosting (Posit Connect)

We recommend using **two separate accounts** (or service accounts) on Posit Connect to host the applications.

### Why two accounts?
Think of this like having two separate phone plans.
*   **The Internal App** processes large Excel files (CEDARS data) and uses a lot of "active hours" (computational time).
*   **The Public App** is for general use and needs to be always available.
*   By separating them, heavy use of the Internal App won't use up the monthly allowance for the Public App, ensuring it stays online for external partners.

### Step 1: Create the Accounts
You will need to sign up for two accounts on shinyapps.io or your internal Posit Connect server. Suggested names:

1.  **`foodbook-public-host`**: This account will only host the Public App (`app-public`).
2.  **`foodbook-internal-host`**: This account will only host the Internal App (`app-internal`).

### Step 2: Connect RStudio to Your Accounts
Now we need to tell RStudio on your computer how to talk to these new accounts.

1.  Log in to your **Public** account dashboard.
2.  Go to **Account** -> **Tokens** -> **Show**.
3.  Copy the command that looks like `rsconnect::setAccountInfo(...)`.
4.  Open RStudio, paste that command into the **Console** (bottom-left pane), and press Enter.
5.  **Repeat steps 1-4 for the Internal account.**

You now have both accounts connected to your RStudio!

---

## Part 3: Deploying the Apps

Now we will upload (deploy) the apps to their respective homes.

### Deploy the Public App
1.  In RStudio, verify you have the project open (`foodbook-shiny-app.Rproj`).
2.  Copy and paste the following code into the RStudio **Console** and press Enter:

    ```r
    library(rsconnect)

    rsconnect::deployApp(
      appDir = ".",
      # These are all the files the Public App needs:
      appFiles = c(
        "app-public/app.R",
        "src/foodbook_backend.R",
        "src/i18n_helper.R",
        "src/app_public_helpers.R",
        "src/common_ui.R",
        "src/common_server.R",
        "src/backend/",
        "src/modules/",
        "translations/translation.json",
        "data/open-canada/"
      ),
      appPrimaryDoc = "app-public/app.R",
      appName = "foodbook-public",
      account = "foodbook-public-host",  # <--- Deploys to the PUBLIC account
      forceUpdate = TRUE
    )
    ```

### Deploy the Internal App
1.  Copy and paste the following code into the RStudio **Console** and press Enter:

    ```r
    library(rsconnect)

    rsconnect::deployApp(
      appDir = ".",
      # These are all the files the Internal App needs:
      appFiles = c(
        "app-internal/app.R",
        "src/foodbook_backend.R",
        "src/i18n_helper.R",
        "src/common_ui.R",
        "src/common_server.R",
        "src/backend/",
        "src/modules/",
        "translations/translation.json",
        "data/open-canada/"
        # Note: If you are using legacy .dta files, uncomment the line below:
        # "upgrade-context/"
      ),
      appPrimaryDoc = "app-internal/app.R",
      appName = "foodbook-internal",
      account = "foodbook-internal-host",  # <--- Deploys to the INTERNAL account
      forceUpdate = TRUE
    )
    ```

---

**Success!** You should now have two live URLs, one for each app, running on separate accounts to maximize stability.
