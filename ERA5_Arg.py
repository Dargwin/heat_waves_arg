import cdstoolbox as ct

@ct.application(title="Max daily temperature - Argentina (0.1° x 0.1°)", description="Choose a year and click the link below to download the gridded data.")
@ct.input.dropdown('year', label='Year', values=range(1992, 2022))
#@ct.input.slider( min=1992 , max=2022, name='year')
@ct.output.download()
def application(year):

   # Retrieve the hourly 2m temperature over ARG for the selected year
    temperature = ct.catalogue.retrieve(
        'reanalysis-era5-land',
        {
            'variable': '2m_temperature',
            #'product_type': 'reanalysis',
            'year': year,
            'month': list(range(1, 12 + 1)),
            'day': list(range(1, 31 + 1)),
            'time': 'ALL',
            'grid': [0.1, 0.1],
            'area': [-57, -76, -21, -52], 
            # retrieve data for Cordoba only
            #ymin, xmin, ymax,xmax
        }
    )
#   cba -50.05, -39.95, -70.05, -24.95  (xmin, xmax, ymin, ymax)
    
    # Compute the daily mean temperature over Europe
    #https://cds.climate.copernicus.eu/toolbox/doc/how-to/14_how_to_resample_and_aggregate/14_how_to_resample_and_aggregate.html
    temperature_daily_max = ct.cube.resample(temperature, freq='day', how='max')

    return temperature_daily_max