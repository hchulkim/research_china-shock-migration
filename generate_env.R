
if (!require(here)) {
	# if using local machine that has here R package
	path_default_nix = "."
} else {
	# if you don't have here installed or if you are using Docker
	library(here)

	path_default_nix <- here() 	
}

library(rix)

rix(date = "2025-09-09",
    r_pkgs = c("languageserver", "pacman", "rix", "here", "data.table", "dplyr", "ggplot2", "sf", "comtradr", "countrycode", "did2s", "fixest", "texreg", "broom", "haven", "fs", "stringr", "glue", "purrr", "tidyr", "readr", "readxl", "zoo", "kableExtra", "bit64", "tidyfast", "tradestatistics", "rmapshaper"),
    system_pkgs = NULL,
    git_pkgs = list( 
	    list(
		    package_name = "concordance",
		    repo_url = "https://github.com/insongkim/concordance",
		    commit = "de8517e0fa2b6cd0de8c02bcb7ba4583cde76aa7"
		    ),
	    list(
	            package_name = "ssaggregate",
		    repo_url = "https://github.com/kylebutts/ssaggregate",
		    commit = "22df93980250891a0cc247f6020136cd33c65ba2"
		    )
		    ),
    ide = "none",
    project_path = path_default_nix,
    overwrite = TRUE,
    print = TRUE)

