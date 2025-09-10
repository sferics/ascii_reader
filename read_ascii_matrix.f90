!========================================================================
!  Author     : Juri Hubrig
!  Date       : 2025-09-10
!  Company    : METEO SERVICE weather research
!  Purpose    : Reads a matrix table, prints the content and recalculates
!               row/column sums for integrity checking (output matches input)
!========================================================================

PROGRAM read_ascii_matrix
    IMPLICIT NONE

    INTEGER, PARAMETER :: max_rows = 100, max_cols = 20
    INTEGER :: iunit, ios, i, j, nrow, ncol, len, arg_len
    CHARACTER(len=256) :: filename, line
    CHARACTER(len=32) :: row_labels(max_rows), col_labels(max_cols)
    REAL :: matrix(max_rows, max_cols)
    REAL :: row_sum(max_rows), col_sum(max_cols)
    REAL :: total_sum
    CHARACTER(len=32) :: tokens(max_cols+2)
    CHARACTER(len=1024) :: buffer, tmp_str

    ! Get filename or use default
    CALL GET_COMMAND_ARGUMENT(1, filename, LENGTH=arg_len)
    IF (arg_len == 0) THEN
        filename = 'matrix.txt'
        !PRINT '(A)', 'No filename supplied; reading default matrix.txt'
    ELSE
        filename = filename(:arg_len)
    END IF

    ! Open and check file
    OPEN(NEWUNIT=iunit, FILE=filename, STATUS='OLD', &
         ACTION='READ', IOSTAT=ios)
    IF (ios /= 0) THEN
        PRINT '(A)', "Error opening file: " // TRIM(filename)
        STOP
    END IF

    ! Read header and separator lines and print them to replicate the input
    READ(iunit, '(A)', IOSTAT=ios) line  ! e.g. Obs \ For ...
    PRINT '(A)', TRIM(line)
    READ(iunit, '(A)', IOSTAT=ios) line  ! separator
    PRINT '(A)', TRIM(line)

    ! Read column label line
    READ(iunit, '(A)', IOSTAT=ios) line
    CALL split_line(line, tokens, len)
    ncol = len - 2     ! skip row label and "Row_Sum"
    DO j = 1, ncol
        col_labels(j) = tokens(j+1)
    END DO
    PRINT '(A)', TRIM(line)

    ! Read the matrix rows and print each original line
    nrow = 0
    DO
        READ(iunit, '(A)', IOSTAT=ios) line
        IF (ios /= 0) EXIT
        IF (INDEX(ADJUSTL(line), 'Col_Sum') == 1) EXIT  ! stop at summary row

        CALL split_line(line, tokens, len)
        IF (len < ncol + 2) CYCLE   ! skip malformed lines
        nrow = nrow + 1
        row_labels(nrow) = tokens(1)
        DO j = 1, ncol
            READ(tokens(j+1), *, IOSTAT=ios) matrix(nrow, j)
            IF (ios /= 0) matrix(nrow, j) = 0.0
        END DO

        ! Print original line as is
        PRINT '(A)', TRIM(line)
    END DO

    CLOSE(iunit)

    ! Calculate row sums and column sums
    DO i = 1, nrow
        row_sum(i) = 0.0
        DO j = 1, ncol
            row_sum(i) = row_sum(i) + matrix(i, j)
        END DO
    END DO
    DO j = 1, ncol
        col_sum(j) = 0.0
        DO i = 1, nrow
            col_sum(j) = col_sum(j) + matrix(i, j)
        END DO
    END DO
    total_sum = 0.0
    DO j = 1, ncol
        total_sum = total_sum + col_sum(j)
    END DO

    buffer = '  Col_Sum'   ! 2 spaces before Col_Sum label

    DO j = 1, ncol
        WRITE(tmp_str, '(I6)') NINT(col_sum(j))  ! Integer right aligned in width 6
        buffer = TRIM(buffer)//tmp_str
    END DO

    WRITE(tmp_str, '(I9)') NINT(total_sum)  ! widen field for total sum alignment
    buffer = TRIM(buffer)//tmp_str

    PRINT '(A)', TRIM(buffer)

    CONTAINS

    ! Splits a line into whitespace tokens
    SUBROUTINE split_line(line, tokens, ntok)
        CHARACTER(len=*), INTENT(IN) :: line
        CHARACTER(len=*), DIMENSION(:), INTENT(OUT) :: tokens
        INTEGER, INTENT(OUT) :: ntok
        CHARACTER(len=256) :: tmp
        INTEGER :: pos, istart, iend
        ntok = 0
        tmp = line
        pos = 1
        DO
            istart = pos
            DO WHILE (istart <= LEN_TRIM(tmp) .AND. tmp(istart:istart) == ' ')
                istart = istart + 1
            END DO
            IF (istart > LEN_TRIM(tmp)) EXIT
            iend = istart
            DO WHILE (iend <= LEN_TRIM(tmp) .AND. tmp(iend:iend) /= ' ')
                iend = iend + 1
            END DO
            ntok = ntok + 1
            tokens(ntok) = tmp(istart:iend-1)
            pos = iend + 1
        END DO
    END SUBROUTINE split_line

END PROGRAM read_ascii_matrix
