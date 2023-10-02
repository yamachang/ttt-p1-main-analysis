# ttt-p1-main-analysis

This repository contains analysis code for this project on the Open Science Framework (OSF): [https://osf.io/c4e75/](https://osf.io/c4e75/).

## Data Cleaning

### Data

#### Raw

Raw data are stored on the [jslab](https://www.schleiderlab.org/) shared drive.

##### From Qualtrics

Raw pre-post data are stored in the `:\\TRACK to TREAT\\Data\\Qualtrics Data\\Raw Data` folder, which contains 18 CSV files obtained from Qualtrics via TODO.
- TODO: How were the files obtained from Qualtrics (just downloaded or was a script used)?

##### From LifePak

Raw ecological momentary assessment (EMA) data are stored in the `:\TRACK to TREAT\Data\LifePak Raw Data (Do Not Modify)` folder, which contains 10 CSV files obtained from LifePak via TODO.
- TODO: How were the files obtained from LifePak (just downloaded or was a script used)?

#### Clean

TODO

### Code

Qualtrics data were cleaned before LifePak data

#### `ttt_phase1_qualtrics_cleaning.Rmd` or `ttt_phase1_qualtrics_cleaning_fix.Rmd` (not yet on repo)

Inputs the following 12 raw CSV files (out of the 18 from Qualtrics)
- `dp5_b_child_p1_numeric.csv`
- `dp5_b_child_p1_choice_text.csv`
- `dp5_b_child_remote_p1_numeric.csv`
- `dp5_b_child_remote_p1_choice_text.csv`
- `dp5_b_parent_p1_numeric.csv`
- `dp5_b_parent_p1_choice_text.csv`
- `dp5_b_parent_remote_p1_numeric.csv`
- `dp5_b_parent_remote_p1_choice_text.csv`
- `dp5_3m_child_p1_numeric.csv`
- `dp5_3m_child_p1_choice_text.csv`
- `dp5_3m_parent_p1_numeric.csv`
- `dp5_3m_parent_p1_choice_text.csv`

Also inputs `dp5_p1_scoring.csv`
- TODO: Where is `dp5_p1_scoring.csv` created? It's not in the `TRACK TO TREAT` folder above (or elsewhere on `jslab` shared drive). Isaac said it's used to create composite scores and he didn't create it. Asked Arielle/Laura on 10/2/23.

Outputs (though both are commented out) `yb_lsmh_ids_dates.csv` and `cleaned_qualtrics_ttt_phase_1.csv`. Moreover, `ttt_phase1_qualtrics_cleaning.Rmd` outputs `cleaned_qualtrics_ttt_phase_1_fixed_220604.csv`, whereas `ttt_phase1_qualtrics_cleaning_fix.Rmd` (which has various hard-coded changes) outputs `cleaned_qualtrics_ttt_phase_1_fixed_220606.csv`.
- TODO: Is `cleaned_qualtrics_ttt_phase_1_fixed_220604.csv` or `cleaned_qualtrics_ttt_phase_1_fixed_220606.csv` ever used later? Seems later scripts just input `cleaned_qualtrics_ttt_phase_1.csv`. Should the later scripts be updated? Isaac said that he recalls revising the cleaning script in May 2022 just to keep a variable that had been deleted or something like that.

#### `ttt_phase1_lifepak_cleaning.Rmd` (not yet on repo)

Inputs the following 5 raw CSV files (out of the 10 from LifePak)
- TODO: What are the file names of the 5 CSV files of LifePak data retained via hard-coding in Line 109?
- TODO
- TODO
- TODO
- TODO

Also inputs `cleaned_qualtrics_ttt_phase_1.csv`

Outputs `cleaned_combined_qualtrics_lifepak_ttt_phase_1.csv`, `cleaned_lifepak_ttt_phase_1.csv`, and `cleaned_qualtrics_ttt_phase_1.csv`

#### `deid_ttt_phase_1.Rmd` (not yet on repo)

Inputs cleaned LifePak data in `cleaned_lifepak_ttt_phase_1.csv` and cleaned Qualtrics data in `cleaned_qualtrics_ttt_phase_1.csv`

Outputs deidentified data in `deid_cleaned_lifepak_ttt_phase_1.csv` and `deid_cleaned_qualtrics_ttt_phase_1.csv`

## Network Analyses

### Data

Analyses input cleaned and deidentified LifePak data in `deid_cleaned_lifepak_ttt_phase_1.csv`

### Code

Initial network analyses were run by [Sebastian Castro-Alvarez](https://github.com/secastroal) and Laura
Bringmann using code in the `from_sebastian/` folder. Analyses were revised by Josip Razum and [Jeremy Eberle](https://github.com/jwe4ec).

TODO

## Machine Learning

### Data

Analyses input cleaned (but not deidentified) LifePak data in `cleaned_lifepak_ttt_phase_1.csv` and cleaned (but not deidentified) Qualtrics data in `cleaned_qualtrics_ttt_phase_1.csv`.
- TODO: Consider updating `ttt_phase_1_main_analyses_08232023_final.Rmd` to input deidentified data in `deid_cleaned_lifepak_ttt_phase_1.csv` and `deid_cleaned_qualtrics_ttt_phase_1.csv`

### Code

Initial machine learning analyses were run by [Michael Mullarkey](https://github.com/mcmullarkey) using
code located at TODO. Analyses were revised by [Yama Chang](https://github.com/yamachang).

#### `ttt_phase_1_main_analyses_08232023_final.Rmd` (in progress and not yet on repo)

Inputs `cleaned_lifepak_ttt_phase_1.csv`, `cleaned_qualtrics_ttt_phase_1.csv`, and `extracted_features_r.csv`
- TODO: Where is `extracted_features_r.csv` created?

Outputs (though all are commented out) `train_long_ema.csv`, `cdi_data_init.csv`, and `analytical_base_table.csv`

#### `Extract feature.ipynb` (in progress and not yet on repo)

Inputs `train_long_ema.csv`, `test_long_ema.csv`, and `extracted_features_r.csv`
- TODO: Where are `test_long_ema.csv` and `extracted_features_r.csv` created?

Outputs `extracted_features_train_0908.csv` and `extracted_features_test_0908.csv`