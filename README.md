# Replicating Treatment Effects in a Colombian Cash-Transfer Experiment

This repository reproduces a transparent baseline subset of the attendance
analysis in Barrera-Osorio, Bertrand, Linden, and Perez-Calle (2011), *Improving
the Design of Conditional Transfer Programs: Evidence from a Randomized Education
Experiment in Colombia*.

The canonical workflow uses the authors' official replication package, applies
published sample restrictions, estimates treatment coefficients with
school-clustered HC1 standard errors, and writes a tidy CSV plus run metadata.

## Run it

1. Read [data/README.md](data/README.md), download the official package, and
   unpack it into the documented local path.
2. Restore the declared R dependencies with `renv::restore()`.
3. Run:

   ```sh
   Rscript scripts/replicate_tables.R
   ```

4. With the official package present, verify the deterministic regression
   checks:

   ```sh
   Rscript tests/test_replication.R
   ```

The outputs are written to `outputs/` and deliberately ignored by Git.

## Scope and limits

This is a reproducible baseline replication, not an exact reproduction of every
published table. The legacy `legacy/analysis.R`, `legacy/replication.R`, and
`legacy/report.pdf` are
preserved as historical course-work artifacts; they are not the supported entry
point because they install packages while running, rely on a personal absolute
path, and depend on an unavailable local data file.

The historical ML analysis should be rebuilt only after this baseline is compared
numerically with the authors' supplied Stata programs. Any extension should set
and report random seeds, preserve the paper's treatment/sample definitions, and
separate exploratory heterogeneity analysis from confirmatory replication.

## References

- Article: [American Economic Association](https://www.aeaweb.org/articles?id=10.1257/app.3.2.167).
- Data: [openICPSR project E113783, version V1](https://doi.org/10.3886/E113783V1).

See [NOTICE.md](NOTICE.md) for third-party material and data terms, and
[CITATION.cff](CITATION.cff) for citation metadata.
