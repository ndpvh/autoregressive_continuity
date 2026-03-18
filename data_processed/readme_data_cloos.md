# Summary: Data from Cloos et al. (2024, 2025)

**Datasets:** `dataset_cloos_beep_long.txt` & `dataset_cloos_burst_short.txt` 

- processed dataset after applying the 20-minute window around the prompted time for both conditions 
    - 120-160 minutes for the long (beep) interval and 0-40 minutes for the short (burst) interval
- observations are further filtered to only valid (or effective) pairs
    - lagging the affect responses was done **including** missed measurements
    - all effective pairs are therefore for truly consecutive measurements, and not for consecutive, non-missing measurements
    - the resulting pairs of measurements are further subsetted to only those, where both, the response and the lagged response, are non-missing
- observations are further filtered that only participants that have at least 20 effective pairs per condition are included in the data

**Key variables**

- `id`: participant identifier
- `connectionId`: equivalent with `id`
- `date`: response date
- `condition`: measurement protocol (`beep` or `burst`), where `beep` is the *long* interval and `burst` is the *short* interval
- `meas_no`: measurement number within `condition`
- `time`: POSIXct timestamp of the response
- `time_lag`: POSIXct timestamp of the lagged response (time of `na_lag` and `pa_lag`)
- `time_interval`: computed difference in time between the response and the lagged response (minutes)
- `na`: negative affect at the current prompt
- `na_lag`: negative affect at the previous prompt (lagged within `id`, `date` & `condition`)
- `pa`: positive affect at the current prompt
- `pa_lag`: positive affect at the previous prompt (lagged within `id`, `date` & `condition`)

**Notes for AR analyses**
- For autoregressive models use `pa` ~ `pa_lag` (and `na` ~ `na_lag`) as the core predictors.