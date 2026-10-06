# Coal-power retirement: figure-generation code

R scripts for figure generation and associated statistical summaries supporting
**Local heat-exposure reductions and economic co-benefits of global coal-power retirement**.

## Scope

This release contains 21 R scripts collected from the manuscript figure and source-data folders on 7 October 2026. It supersedes the broader `Figure_R0927.zip` plotting collection for identifying the scripts in this figure package. The scripts are copied without changes to their calculations or graphical settings.

These are plotting, summary-statistics and graphics-export scripts. They do not constitute the complete upstream analysis pipeline: SAM training, temperature-response simulations and GCAM model runs are not included. Scripts that convert or extract existing SVG panels do not regenerate the underlying scientific results.

## Contents

| Directory | Contents |
| --- | --- |
| `scripts/Figure1` | Seven scripts: temperature/heatwave maps and latitude profiles; plant case maps; fuel and CHP comparisons; SVG/PDF utilities |
| `scripts/Figure2` | Five scripts: loss maps, loss composition, settlement contributions, income-group comparisons and city/country rankings |
| `scripts/Figure3` | Five scripts: scenario maps, latitude profiles, avoided-loss insets and quadrant plots |
| `scripts/Supplementary` | Four scripts: SAM training-history plots, facility imagery/boundary examples, temperature maps/profiles and final-panel exports |

`script_manifest.csv` maps every script to its source-package location and records its SHA-256 checksum. `input_path_inventory.csv` lists input-reading and configuration statements. `r_packages.txt` lists detected package names; it is not a version lockfile. `syntax_check.csv` records R parsing checks, not successful execution with data.

## Data

Data are distributed separately from this code. The reserved Figshare record is
[10.6084/m9.figshare.34113129](https://doi.org/10.6084/m9.figshare.34113129).
At preparation of this code package, this identifier was reserved and public access to the complete figure source-data collection had not been confirmed. Consult the record's file list for its actual contents; a boundary-only archive is not sufficient to run all scripts.

Required inputs include:

- Figure 1: `Source_Data_Figure1.xlsx`, case temperature rasters and facility boundaries.
- Figure 2: `Source_Data_Figure2.xlsx`, intermediate summary CSV files and the administrative boundary layer used in spatial aggregation.
- Figure 3: `Source_Data_Figure3.xlsx`, intermediate city/scenario CSV files and administrative boundaries.
- Supplementary Figure 1: `source_data/SAM_training_history.csv` (training is not rerun).
- Supplementary Figure 2: four RGB rasters and a compatible facility-boundary file. Set `SI_BOUNDARY_FILE` to the boundary-file path.
- Supplementary Figure 3: `source_data/Source_Data_SI_Fig3.xlsx`.
- SVG export utilities: the corresponding existing SVG panels or final-layout SVG files.

Some inputs are third-party spatial data. Obtain those inputs from the sources described in the manuscript and observe their redistribution conditions.

## Running the scripts

1. Install R and the packages listed in `r_packages.txt`. Spatial and SVG packages may also require system libraries.
2. Obtain the required source data and intermediate files. Keep the supplementary `source_data` folder beside the supplementary scripts, as expected by those scripts.
3. Configure the input and output paths **before running**. Several main-figure scripts retain original absolute paths. Use `input_path_inventory.csv` to locate them and select a new output directory to avoid overwriting previous figures.
4. Check spreadsheet sheet names. Two Figure 2 scripts request `sheet = "coal"`; when using an extracted workbook with `Sheet1`, change the script's sheet argument to the actual sheet name. Do not rename or reinterpret data columns silently.
5. Run the required data-preparation script before its dependent plot. Figure 2's `Figure2_a_b数据` script generates summaries consumed by `Figure2_b`. Figure 3's map outputs feed the latitude-profile workflow, and its `Figure3_ef数据准备` script supplies quadrant CSV inputs. Align their output/input paths.
6. For the coordinated Figure 3 map/inset colours, set `FIG3_STYLE=fig2_harmonized`. Run the applicable scripts for both `SSP126` and `SSP370`. The quadrant plotting script takes the scenario as its first command-line argument and `remaining_top10` as its second argument for the final selection, rather than its default `representative` selection.
7. Run a configured script using `Rscript "path/to/script.R"`. Record `sessionInfo()` in the environment used for reproduction.

The cross-figure utility `Figure1_cdef_Figure2_de_SVG转PDF.R` expects sibling directories named `组图一` and `组图二` beneath its base directory. Adapt that layout/path before use; the GitHub code-only folder does not contain its SVG inputs. The separate Figure 1 final-SVG export script is an alternative for those four panels, not an additional scientific-analysis step.

The Figure 1 case-generation helper also references a plotting template: update both paths together. Figure-panel prefixes identify intended roles, not a universal execution sequence. This collection does not include a single automatic full-figure assembly pipeline.

## Verification and reuse

All 21 copies were checksum-verified against the source folder and passed R syntax parsing during packaging. Full execution of this standalone code-only release has not been validated. Matching final figures also requires the documented data, scenario options and layout inputs.

No new licence is granted by this packaging step. The authors should confirm a code licence before release; any third-party data retain their own terms.
