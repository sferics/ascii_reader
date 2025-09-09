!=======================================================================
!  Author     : Juri Hubrig
!  Date       : 2025-09-08
!  Company    : METEO SERVICE weather research
!  Purpose    : Reads a table file and loads columns into arrays
!=======================================================================

SUBROUTINE read_table(filename, nmax, nrows, Kl, MFc, MOb, count_vals)
    IMPLICIT NONE
    CHARACTER(len=*), INTENT(IN) :: filename
    INTEGER, INTENT(IN) :: nmax
    INTEGER, INTENT(OUT) :: nrows
    REAL, INTENT(OUT) :: Kl(nmax), MFc(nmax), MOb(nmax)
    INTEGER, INTENT(OUT) :: count_vals(nmax)
    INTEGER :: iunit, ios, i
    CHARACTER(len=256) :: line

    ! Open file for reading, handle errors if file is absent
    OPEN(NEWUNIT=iunit, FILE=filename, STATUS='OLD', &
         ACTION='READ', IOSTAT=ios)
    IF (ios /= 0) THEN
        PRINT '(A)', "Error opening file: " // TRIM(filename)
        nrows = 0
        RETURN
    END IF

    ! Skip first two header lines
    READ(iunit, '(A)', IOSTAT=ios) line
    READ(iunit, '(A)', IOSTAT=ios) line

    i = 0
    DO
        ! Read data into arrays, stop at end-of-file or on error
        READ(iunit, *, IOSTAT=ios) Kl(i+1), MFc(i+1), MOb(i+1), &
             count_vals(i+1)
        IF (ios /= 0) EXIT
        i = i + 1
        IF (i >= nmax) EXIT
    END DO

    nrows = i
    CLOSE(iunit)
END SUBROUTINE read_table

PROGRAM read_ascii
    IMPLICIT NONE

    CHARACTER(len=256) :: filename, arg
    INTEGER :: nmax, nrows, i, arg_len
    REAL, ALLOCATABLE :: Kl(:), MFc(:), MOb(:)
    INTEGER, ALLOCATABLE :: count_vals(:)
    LOGICAL :: show_help

    ! Get the first command-line argument, if present
    CALL GET_COMMAND_ARGUMENT(1, arg, LENGTH=arg_len)

    ! Check if argument is -h or --help (user requests usage information)
    show_help = .FALSE.
    IF (arg_len > 0) THEN
        arg = arg(:arg_len)
        IF (TRIM(arg) == '-h' .OR. TRIM(arg) == '--help') show_help = .TRUE.
    END IF

    ! If help requested, print usage and exit
    IF (show_help) THEN
        PRINT '(A)', "Usage: ./read_ascii [filename]"
        PRINT '(A)', "Reads a table file (default: data.txt) with columns:"
        PRINT '(A)', "Kl, MFc, MOb, #"
        PRINT '(A)', "File format must have two header lines, followed by data."
        PRINT '(A)', "Example line:"
        PRINT '(A)', "  0.0   0.00   2.73   400"
        PRINT '(A)', "If no filename is supplied, reads data.txt by default."
        STOP
    END IF

    ! If a filename argument was given, use it. Otherwise set "data.txt".
    IF (arg_len > 0 .AND. .NOT. show_help) THEN
        filename = arg(:arg_len)
    ELSE
        filename = 'data.txt'
    END IF

    nmax = 1000
    ALLOCATE(Kl(nmax), MFc(nmax), MOb(nmax), count_vals(nmax))

    CALL read_table(filename, nmax, nrows, Kl, MFc, MOb, count_vals)

    !PRINT '(A)', "Rows read: " // TRIM(adjustl(write_int(nrows)))
    PRINT '(A)', "  Kl      MFc    MOb    # "
    PRINT '(A)', "-----  ------ ------  ----"
    DO i = 1, nrows
        PRINT '(F5.1, F8.2, F7.2, I6)', Kl(i), MFc(i), MOb(i), &
              count_vals(i)
    END DO

CONTAINS

    ! Converts integer to string for output concatenation
    FUNCTION write_int(i) RESULT(str)
        INTEGER, INTENT(IN) :: i
        CHARACTER(len=20) :: str
        WRITE(str, '(I0)') i
    END FUNCTION write_int

END PROGRAM read_ascii
