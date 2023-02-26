# Analyses of Land Snails from Gran Canaria

This repository contains code to support and make analyses of snail populations on Gran Canaria.

## Data

The data used come from surveys conducted by professional and amateur researchers on Gran Canaria.

## Analysis

The analysis code is stored in markdown `.Rmd` files assisted with separate `.R` scripts.

## Repo structure

```
├── README.md                  : Description of this repository
├── gran-canaria-snails.Rproj  : RStudio project file
├── .gitignore                 : Files and directories to be ignored by git
│
├── data
│   ├── raw                    : Source data
│   └── processed              : Data after processing trough script
│
└── src
    ├── R                      : R scripts with helper functions
    └── markdown               : (R)markdown scripts with analysis and their descriptions
```

## Contributors

[Ward Langeraert](https://github.com/wlangera)

## License

[MIT License](LICENSE) for the code and documentation in this repository. The included data is released under another license.