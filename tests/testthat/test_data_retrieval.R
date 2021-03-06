context("Data retrieval from web")

library(yaml)
yml = yaml.load_file(system.file(package='sars2app', path='data_catalog/dataset_details.yaml'))
dsets = yml$datasets

for(dset in names(dsets)) {
    #if(dset=='apple_mobility_data') next ## skip for now
    accessor = get(dset)
    res = accessor()
    # skip non-data-frame-like objects for now
    if(inherits(res,'data.frame')) {
        cnames = colnames(res)
        test_that(paste0(dset, " column names match"), 
                  expect_equal(cnames, names(dsets[[dset]]$columns)))
        ctypes = lapply(res, class)
        test_that(paste0(dset, " column types match"),
                  expect_equal(ctypes, dsets[[dset]]$columns))
    }
}
