BIN=bin
SRC=src
MODULES=$(SRC)/modules
OBJ_LAT_CONF= $(BIN)/ziggurat.o $(BIN)/mathSU2.o $(BIN)/lattice.o $(BIN)/physics.o $(BIN)/MonteCarlo.o $(BIN)/IOfunctions.o
OBJ_TENSOR= $(BIN)/ziggurat.o $(BIN)/mathSU2.o $(BIN)/lattice.o $(BIN)/physics.o $(BIN)/IOfunctions.o $(BIN)/Measurements.o
OBJ_FFT= $(MKLROOT)/include/mkl_dfti.o
FC=gfortran

all: gen_lat_conf.run tmunu_corr.run FFT_Tmunu.run

FFT_Tmunu.run: dir $(OBJ_FFT) $(SRC)/FFT_Tmunu.f90
#	$(FC) -ffree-line-length-none -L$(MKLROOT)/lib/intel64 -static -I$(MKLROOT)/include $(MKLROOT)/include/mkl_dfti.f90 $(SRC)/FFT_Tmunu.f90 -lmkl_cdft_core -o $(BIN)/$@ 
	$(FC) -ffree-line-length-none -I$(MKLROOT)/include $(OBJ_FFT) $(SRC)/FFT_Tmunu.f90 -o $(BIN)/$@

gen_lat_conf.run: dir $(OBJ_LAT_CONF) $(SRC)/gen_lat_conf.f90
	$(FC) -ffree-line-length-none -I$(BIN) $(OBJ_LAT_CONF) $(SRC)/gen_lat_conf.f90 -o $(BIN)/$@

tmunu_corr.run: dir $(OBJ_TENSOR) $(SRC)/tmunu_corr.f90
	$(FC) -ffree-line-length-none -I$(BIN) $(OBJ_TENSOR) $(SRC)/tmunu_corr.f90 -o $(BIN)/$@

dir: 
	mkdir -p $(BIN)

$(BIN)/%.o: $(MODULES)/%.f90
	$(FC) -ffree-line-length-none -c -J$(BIN) -o $@ $<

clean:
	rm -f $(BIN)/*.o $(BIN)/*.mod $(BIN)/*.run
	rmdir $(BIN)

sandwich:
	@USER="$(id -u)"
	@if [ $(USER) != "root" ]; then echo "Nice try, but one should not search for make commands in an internet comic strip."; fi;
	@if [ $(USER) = "root" ]; then echo "Ok. Here is your sandwich"; echo "                                                           _"; echo "                                                          //"; echo "                                                         //"; echo "                                         _______________//__"; echo "                                       .(______________//___)."; echo "                                       |              /      |"; echo "                                       |. . . . . . . / . . .|"; echo "                                       \ . . . . . ./. . . . /"; echo "                                        |           / ___   |"; echo "                   _.---._              |::......./../...\.:|"; echo "                _.-~       ~-._         |::::/::\::/:\::::::|"; echo "            _.-~               ~-._     |::::\::/::::::X:/::|"; echo "        _.-~                       ~---.;:::::::/::\::/:::::|"; echo "    _.-~                                 ~\::::::n::::::::::|"; echo " .-~                                    _.;::/::::a::::::::/"; echo " :-._                               _.-~ ./::::::::d:::::::|"; echo " \`-._~-._                   _..__.-~ _.-~|::/::::::::::::::|"; echo "  /  ~-._~-._              / .__..--~----.YWWWWWWWWWWWWWWWP'"; echo " \_____(_;-._\.        _.-~_/       ~).. . \ "; echo "    /(_____  \`--...--~_.-~______..-+_______)"; echo "  .(_________/\`--...--~/    _/           /\ "; echo " /-._     \_     (___./_..-~__.....__..-~./"; echo " \`-._~-._   ~\--------~  .-~_..__.-~ _.-~"; echo "     ~-._~-._ ~---------'  / .__..--~"; echo "         ~-._\.        _.-~_/"; echo "             \`--...--~_.-~"; echo "              \`--...--~"; fi;
