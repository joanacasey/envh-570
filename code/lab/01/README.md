# Lab Orientation

## Frequently Asked Questions (FAQ)

### Why?

- Why are we using RStudio "Server"?
  - To provide a shared R environment, ready for use, for the whole class.
- Why are we using Github?
  - To make it easy to get course materials into R, as well as easy updates.
- Why do we make a copy of a provided file before editing? 
  - So the the original files from Github can be updated without conflicts.
- Why are we using "pacman" and "pak"?
  - These alternatives to install.packages() and library() are smarter and 
    easier to use.
    - "pacman" will load multiple packages, installing as needed.
    - "pak" is an upgrade to install.packages(), that is fast and smart about 
      resolving dependencies, or at least is better at explaining how to 
      resolve them yourself.
- Why are we using "here"?
  - here() is a smarter alternative to file.path() because it knows the top 
    level of your project, making it easier to refer to files and folders.

### How?

- How do we transfer files to/from RStudio Server?
  - Use the "Upload" and "More" -> "Export..." buttons in the "Files" pane 
    to upload and download files (respectively).

### What?

- What "best practices" are we trying to follow?
  - Consistent style (indentation, punctuation, commenting, line width, etc.).
  - Reproducibility (portability, scripted automation, etc.).
  - Use of version control (centralized location, co-authoring, change tracking).
- What are we doing for better reproducibility?
  - Scripts are self-contained, or only refer to files which are in the Git 
    repository. All files (code and data) included in the repository are all 
    you need to reproduce the analysis.
  - Scripts will install packages for you if any are missing from your system.
  - Scripts are written with platform-neutral, relative file paths for 
    greater portability.
- What guidelines for coding style are we following?
  - We are trying to follow [Tidyverse Style Guide](https://style.tidyverse.org/index.html).
- What RStudio settings are generally recommended?
  - See [Customization and Global Options](https://github.com/deohs/r_onramp/blob/main/Introduction.md#customization-and-global-options).
  - We also recommend Tools -> Global Options -> General -> Workspace
    - [ ] (uncheck) Restore .RData into workspace at startup
    - Save workspace to .RData on exit: [Never]
- What are some additional R and coding help resources?
    - See: [Resources and getting help](https://github.com/deohs/r_onramp/blob/main/r_resources_and_getting_help.md).
