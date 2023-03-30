# Force install git version of reticulate due to SSL related bug in CRAN version
# one-time install only
#remotes::install_github("rstudio/reticulate")
#remotes::install_github("ropensci/rtweet")

# Installs miniconda for virtual environment deployment
#reticulate::install_miniconda()

# Creates a virtual enviroment "r-sentiment-ai" based on miniconda
# Every call for sentiment.ai functions is passed through here
# Python version must be 3.8.10 for compatibility reasons with required packages
# one-time creation only
#reticulate::virtualenv_create(envname = "r-sentiment-ai",python_version = "3.8.10")

# Setup virtual enviroment with packages required
# one-time setup only
#install_sentiment.ai()

# Initialise the sentiment ai model
# Must be initiated before starting calling sentiment.ai
# Model en.large is default; considerable resource use but most accurate for EN Lang
# Alternative models are...
# init_sentiment.ai(model = "en.large",envname = "r-sentiment-ai")
