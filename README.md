# Research software for averting the steel carbon lock-in through strategic green investments.
## Overview
This research software was used to obtain results for the article **Averting the steel carbon lock-in through strategic green investments** (Link TBD).

It analyses both data from the Global Energy Monitor *Iron and Steel Plant Tracker* and scenario results from the REMIND model. 
All figures from the article can be directly reproduced using this software, which calculates yearly and cumulative emissions, average abatement costs, and investment costs required for the steel sector. 

## How to use this software

### System requirements

This software can be run on a standard computer capable of running standard Python and R scripts.
All major operating systems can be used (Windows, MacOS or Linux), although this software has only been tested on Windows 10.

### Installing Python and R dependencies

All required dependencies are installed automatically when running the appropriate sections of the R Markdown script: 
```
plot_figures.Rmd
```
No separate environment setup is required.
### Replicating figures

To reproduce the figures from the article:
1. Open the R markdown file
```
plot_figures.Rmd
```
2. Run the section corresponding to the figure you wish to replicate. Note that some figures rely on intermediate results generated in earlier sections; if these are not available, the code will throw an error.

## Input data structure

The Global Energy Monitor *Iron and Steel Plant Tracker* data (July 2025 release) is stored in `inputdata/python_source`. The folder includes a correction table for some missing data in the original files, which was found directly on the [GEM Wiki](https://www.gem.wiki/Main_Page). 
This data was then analysed using the python source code (`analysis/python`), and the output was saved in `inputdata/gem`. 

The REMIND model provides scenario results under two file formats, raw (.gdx) and processed (.mif). Both types are used in the analysis code, and the corresponding scenario data can be found under `inputdata/gdx` and `inputdata/mif`.

Finally, `inputdata/worldsteel` contains processed data from the World Steel Association on historical production, courtesy of Merlin Hosak.


## How to cite this work

Bachorz, Clara; Dürrwächter, Jakob; Gong, Chen Chris; Odenweller, Adrian; Pehl, Michaja; Schreyer, Felix; Verpoort, Philipp C.; Ueckerdt, Falko; Luderer, Gunnar: Research software for averting the steel carbon lock-in through strategic green investments.

## License
The code contained in this repository is available for use under an [MIT license](https://opensource.org/license/mit).
