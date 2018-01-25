BIN=bin
SRC=src
MODULES=$(SRC)/modules
OBJ= $(BIN)/ziggurat.o $(BIN)/mathSU2.o $(BIN)/lattice.o $(BIN)/physics.o $(BIN)/MonteCarlo.o $(BIN)/IOfunctions.o
FC=gfortran

all: gen_lat_conf.run

gen_lat_conf.run: dir $(OBJ) $(SRC)/gen_lat_conf.f90
	$(FC) -ffree-line-length-none -I$(BIN) $(OBJ) $(SRC)/gen_lat_conf.f90 -o $(BIN)/gen_lat_conf.run

dir: 
	mkdir -p $(BIN)

$(BIN)/%.o: $(MODULES)/%.f90
	$(FC) -ffree-line-length-none -c -J$(BIN) -o $@ $<

clean:
	rm $(OBJ) $(BIN)/*.mod $(BIN)/*.run
