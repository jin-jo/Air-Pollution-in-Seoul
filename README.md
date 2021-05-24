# Air-Pollution-in-Seoul
## Air Pollution Measurement Information in Seoul, Korea

### Context
This dataset deals with air pollution measurement information in Seoul, South Korea.
Seoul Metropolitan Government provides many public data, including air pollution information, through the [Open Data Plaza](https://data.seoul.go.kr/).
I used air pollution related datasets provided by the Seoul Metropolitan Government. 

### Content
This data provides average valus for six pollutants (O2, NO2, CO, O3, PM10, PM2.5).
- Data were measured every hour between 2017 and 2019.
- Data were measured for 25 districts in Seoul.
- This dataset is divided into four files.

1. **Measurement info**: Air pollution measurement information
    - 1 hour average measurement is provided after calibration
    - Instrument status:
      - `0`: Normal, `1`: Need for calibration, `2`: Abnormal
      - `4`: Power cut off, `8`: Under repair, `9`: abnormal data
2. **Measurement item info**: Information on air pollution measurement items
3. **Measurement station info**: Information on air pollution instrument stations
4. **Measurement summary**: A condensed dataset based on the above three data.
