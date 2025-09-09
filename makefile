FC = ifort
FFLAGS = -O2
TARGET = read_ascii

all: $(TARGET)

$(TARGET): read_ascii.f90
	$(FC) $(FFLAGS) -o $(TARGET) read_ascii.f90

clean:
	rm -f $(TARGET) *.mod *.obj *.o
