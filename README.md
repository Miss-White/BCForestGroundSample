<!-- README.md is generated from README.Rmd. Please edit that file -->
BCForestGroundSample hosts functions for compiling forest ground sample data. Currently, it supports the functions used in VRI Compiler. It Will incorporate PSP compiler soon.

Package Status
--------------

This package is still under development.

Development History and Features
--------------------------------

### version 1

**Date**: To March 1st, 2018

**Features**:

-   clarified the unnecessary process in SAS compiler
-   implemented mid-point guess in height estimate for broken top trees
-   added lorey's height estimate in height summary
-   added tree flag for
-   full measured trees (DBH + Height + Call grading available)
-   enhanced trees in auxi plot (DBH + Height + Call grading available)
-   height enhanced trees (DBH + Height available)
-   non-enhanced trees (DBH available)
-   outputed data for deriving regression and ratio
-   hard-coded lookup tables (SAS compiler reads these tables)

### version 2

**Date**: To present

**Features**:

-   specified trees in B type plot as H enhanced trees, as their call grading information is not available
-   introduced SIndexR package to remove the sas-dependency of sindexdll

Issue/Bug Reporting
-------------------

Please file an [issue](https://github.com/bcgov/BCForestGroundSample/issues/), if you encounter.

Install package
---------------

The package can be installed from github by using following codes:

``` r
library(devtools)
install_github("bcgov/BCForestGroundSample")
```

**Warning messages** may be showed up when you install the package. However, it should not affect outputs of each function. If you have any concerns about the package, please report your issue [here](https://github.com/bcgov/BCForestGroundSample/issues).

Use package
-----------

The below codes demonstrate some examples use the functions in this package:

Contribute to the package
-------------------------

If you would like to contribute to the package, please see our [CONTRIBUTING](https://github.com/bcgov/BCForestGroundSample/blob/master/CONTRIBUTING.md) guidelines.

Please note that this project is released with a [Contributor Code of Conduct](https://github.com/bcgov/BCForestGroundSample/blob/master/CODE_OF_CONDUCT.md). By participating in this project you agree to abide by its terms.

Licence
-------

    # Copyright 2018 Province of British Columbia
    # 
    # Licensed under the Apache License, Version 2.0 (the "License");
    # you may not use this file except in compliance with the License.
    # You may obtain a copy of the License at
    # 
    # http://www.apache.org/licenses/LICENSE-2.0
    # 
    # Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
    # WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    # See the License for the specific language governing permissions and limitations under the License.
