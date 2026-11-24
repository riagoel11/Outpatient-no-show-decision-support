# Data

The raw data files are not stored in this repository. Both are openly available from their publishers. Download them and place them in your R working directory (the scripts read them by file name).

| File expected by the code | Source | Used for |
|---|---|---|
| `KaggleV2-May-2016.csv` | [Medical Appointment No Shows – Kaggle (Joni Hoppen / Aquarela Analytics)](https://www.kaggle.com/datasets/joniarroba/noshowappointments) | Primary modelling dataset: 110,527 outpatient appointments, Vitória (Brazil), 2016 |
| `hosp-epis-stat-outp-rep-tabs-2024-25-tab.xlsx` | [Hospital Outpatient Activity 2024-25 – NHS England (Hospital Episode Statistics)](https://digital.nhs.uk/data-and-information/publications/statistical/hospital-outpatient-activity/2024-25) – "Outpatient reporting tables" | External comparison only (aggregate NHS DNA rates by year and age band) |

No identifiable patient records were used. The NHS tables are aggregate, published statistics.
