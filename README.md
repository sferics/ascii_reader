# Table of Contents

- [ascii_reader](#ascii_reader)
    - [Overview](#overview)
    - [Usage](#usage)
    - [Input File Format](#input-file-format)
    - [Output](#output)
- [ascii_reader_matrix](#ascii_reader_matrix)
    - [Overview](#overview-1)
    - [Usage](#usage-1)
    - [Input File Format](#input-file-format-1)
    - [Output](#output-1)
- [Scope and Adaptation](#scope-and-adaptation)

***

# ascii_reader

## Overview

`read_ascii` is a simple Fortran 90 program designed as a **template** for parsing and verifying the import of tabular ASCII data files with a fixed structure. Its main purpose is to read columns of numeric data from an input file, load them into arrays, and confirm successful reading by printing the contents in formatted columns.

The provided code is intended for adaptation—users can modify the parsing routine or extend it for further data handling and analysis.

***

## Usage

### Compiling

Compile using the Intel Fortran compiler:

```bash
ifort -O2 -o read_ascii read_ascii.f90
```


### Running

You can run the program with no argument, or supply the table file as the first argument:

```bash
./read_ascii
```

or

```bash
./read_ascii your_table_file.txt
```

If called without arguments, it defaults to reading `data.txt`.

To show usage info, run:

```bash
./read_ascii -h
```

or

```bash
./read_ascii --help
```


***

## Input File Format

- The file must begin with **two header lines** (title and separator), which will be skipped.
- Each subsequent line must contain **four columns** of numeric data, separated by whitespace:

```
 Kl     MFc   MOb    #
-----  ------ ------  ----
  0.0   0.00   2.73   400
  1.0   0.72   3.10   100
100.0  99.24  87.43   300
```


***

## Output

- The program prints the number of rows read and echoes the columns with fixed formatting.
- These `PRINT` statements are only provided to confirm that the data was correctly parsed.
- For real-world use, adapt or remove the print section and replace with your own data processing logic.

***

# ascii_reader_matrix

## Overview

`read_ascii_matrix` is a Fortran 90 program designed as a **template** for reading and verifying matrix-style ASCII table files with row and column labels plus summation rows/columns.

It loads the matrix data, recalculates row and column sums, and prints the full table with sums to confirm that input parsing and summation integrity are correct.

This program is intended for adaptation in workflows handling matrix-structured ASCII data.

***

## Usage

### Compiling

Compile with Intel Fortran compiler:

```bash
ifort -O2 -o read_ascii_matrix read_ascii_matrix.f90
```


### Running

You may run the program with an optional filename argument:

```bash
./read_ascii_matrix
```

Reads the default file `matrix.txt`.

Or specify a custom file:

```bash
./read_ascii_matrix your_matrix_file.txt
```


### Help

Request usage information with:

```bash
./read_ascii_matrix -h
```

or

```bash
./read_ascii_matrix --help
```


***

## Input File Format

- File must include two header lines (a descriptive header and a separator) which are echoed verbatim.
- Followed by a column label line including a row label column and a trailing `Row_Sum` column.
- Subsequent lines contain:
    - Row labels (typically numeric or strings).
    - Matrix data columns (numeric values).
    - Trailing row sum column.
- Ends with a `Col_Sum` row giving column sums and total sum.

Example input snippet:

```
Obs \ For   2.5   5.0   7.5  10.0  12.5  15.0  20.0  30.0  40.0  Row_Sum
---------  ----  ----  ----  ----  ----  ----  ----  ----  ----  -------
      2.5     0     0     0     0     0     0     0     0     0        0
      5.0     0     1     1     0     0     0     0     0     0        2
      ...
  Col_Sum     0     1    23   103    94    52    31     0     0      304
```


***

## Output

- The program reproduces the input file line-by-line (header, separator, labels, data).
- The final `Col_Sum` row is recalculated and printed in the exact format.
- Use this output to confirm the file integrity or as a starting point for further data processing.

***

# Scope and Adaptation

- This template is intended as a **starting point** for further development or as a parser module for larger projects involving ASCII data imports.
- Both readers demonstrate basic file input, error handling, command-line argument parsing, and formatted output in Fortran 90.
- The print statements in each program help confirm that the input file was read correctly before further scientific or business processing.
- These templates can be extended and customized to support other ASCII data formats, variable column counts, or advanced validation as needed.
- Clear and detailed commenting is included to aid maintenance and adaptation by future developers.
