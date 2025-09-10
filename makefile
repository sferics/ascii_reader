FC = ifort
FFLAGS = -O2

TARGETS = read_ascii read_ascii_matrix

all: $(TARGETS)

read_ascii: read_ascii.f90
	$(FC) $(FFLAGS) -o read_ascii read_ascii.f90

read_ascii_matrix: read_ascii_matrix.f90
	$(FC) $(FFLAGS) -o read_ascii_matrix read_ascii_matrix.f90

clean:
	rm -f read_ascii read_ascii_matrix *.mod *.obj *.o
