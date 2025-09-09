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

## Scope and Adaptation

- This template is intended as a **starting point** for further development or as a parser module for larger projects.
- It demonstrates basic file input, error handling, command-line arguments, and formatted output in Fortran 90.
- Comments are provided for clarity and maintainability.

***

## License

You may use, modify, and redistribute this code freely for educational or research purposes. No warranty is provided.

***

**Author:**
Juri Hubrig / METEO SERVICE weather research
September 2025
