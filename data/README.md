# Data acquisition

The canonical input is expected locally at
`data/raw/113783-V1/AEJApp_2010-0132_Data/Public_Data_AEJApp_2010-0132.dta`.

Download it manually from the authors' official openICPSR archive:

- Project DOI: <https://doi.org/10.3886/E113783V1>
- File DOI: <https://doi.org/10.3886/E113783V1-200343>

The package currently present in this local checkout was manually acquired from
the archive and retains its upstream `LICENSE.txt`. The package is ignored by
Git, so anyone reproducing the workflow must download and unpack it locally while
preserving the directory layout above. Its upstream license applies a Modified
BSD License to code and CC BY 4.0 to databases and other materials; review that
license before redistribution.

The script validates the expected schema before estimating anything. The package
is version `V1`, published 2019-10-12. Cite the data as specified on the archive
page.
