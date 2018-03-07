BIN=bin
SRC=src
MODULES=$(SRC)/modules
OBJ_LAT_CONF= $(BIN)/ziggurat.o $(BIN)/mathSU2.o $(BIN)/lattice.o $(BIN)/physics.o $(BIN)/MonteCarlo.o $(BIN)/IOfunctions.o
OBJ_TENSOR= $(BIN)/ziggurat.o $(BIN)/mathSU2.o $(BIN)/lattice.o $(BIN)/physics.o $(BIN)/IOfunctions.o $(BIN)/Measurements.o
OBJ_FFT= $(MKLROOT)/include/mkl_dfti.o
FC=gfortran

ifeq ($(FC),ifort)
MKL_LINK=-mkl
endif
ifeq ($(FC),gfortran)
MKL_LINK=-L${MKLROOT}/lib/intel64 -Wl,--no-as-needed -lmkl_rt -lpthread -lm -ldl
FFLAGS=-ffree-line-length-none
endif


all: gen_lat_conf.run tmunu_corr.run FFT_Tmunu.run orbitAvrg.run statisticalAverager.run

orbitAvrg.run: dir $(SRC)/orbitAvrg.f90
	if [ $(FC) = ifort ]; then  $(FC) -heap-size-1023 $(SRC)/orbitAvrg.f90 -o $(BIN)/$@; elif [ $(FC) = gfortran ]; then $(FC) $(FFLAGS) $(SRC)/orbitAvrg.f90 -o $(BIN)/$@; fi

statisticalAverager.run: dir $(SRC)/statisticalAverager.f90
	if [ $(FC) = ifort ]; then  $(FC) -heap-size-1023 $(SRC)/statisticalAverager.f90 -o $(BIN)/$@; elif [ $(FC) = gfortran ]; then $(FC) $(FFLAGS) $(SRC)/statisticalAverager.f90 -o $(BIN)/$@; fi

FFT_Tmunu.run: dir $(SRC)/FFT_Tmunu.f90
	if [ $(FC) = ifort ]; then $(FC) -heap-size-1023 -I=$(MKLROOT)/include/mkl_dfti.f90 $(SRC)/FFT_Tmunu.f90 $(MKL_LINK) -o $(BIN)/$@; elif [ $(FC) = gfortran ]; then $(FC) -m64 -I${MKLROOT}/include  $(FFLAGS) $(SRC)/FFT_Tmunu.f90 $(MKL_LINK) -o $(BIN)/$@; fi

gen_lat_conf.run: dir $(OBJ_LAT_CONF) $(SRC)/gen_lat_conf.f90
	if [ $(FC) = ifort ]; then $(FC) -heap-size-1023 -I$(BIN) $(OBJ_LAT_CONF) $(SRC)/gen_lat_conf.f90 -o $(BIN)/$@; elif [ $(FC) = gfortran ]; then $(FC) $(FFLAGS) -I$(BIN) $(OBJ_LAT_CONF) $(SRC)/gen_lat_conf.f90 -o $(BIN)/$@; fi

tmunu_corr.run: dir $(OBJ_TENSOR) $(SRC)/tmunu_corr.f90
	if [ $(FC) = ifort ]; then $(FC) -heap-size-1023 -I$(BIN) $(OBJ_TENSOR) $(SRC)/tmunu_corr.f90 -o $(BIN)/$@; elif [ $(FC) = gfortran ]; then $(FC) $(FFLAGS) -I$(BIN) $(OBJ_TENSOR) $(SRC)/tmunu_corr.f90 -o $(BIN)/$@; fi

dir: 
	mkdir -p $(BIN)

$(BIN)/%.o: $(MODULES)/%.f90
	if [ $(FC) = ifort ]; then $(FC) -heap-size-1023 -c -module $(BIN) -o $@ $<; elif [ $(FC) = gfortran ]; then $(FC) $(FFLAGS) -c -J$(BIN) -o $@ $<; fi

clean:
	rm -f $(BIN)/*.o $(BIN)/*.mod $(BIN)/*.run
	rmdir $(BIN)

sandwich:
	@USER="$(id -u)"
	@if [ $(USER) != "root" ]; then echo "Nice try, but one should not search for make commands in an internet comic strip."; fi;
	@if [ $(USER) = "root" ]; then echo "Ok. Here is your sandwich"; echo "                                                           _"; echo "                                                          //"; echo "                                                         //"; echo "                                         _______________//__"; echo "                                       .(______________//___)."; echo "                                       |              /      |"; echo "                                       |. . . . . . . / . . .|"; echo "                                       \ . . . . . ./. . . . /"; echo "                                        |           / ___   |"; echo "                   _.---._              |::......./../...\.:|"; echo "                _.-~       ~-._         |::::/::\::/:\::::::|"; echo "            _.-~               ~-._     |::::\::/::::::X:/::|"; echo "        _.-~                       ~---.;:::::::/::\::/:::::|"; echo "    _.-~                                 ~\::::::n::::::::::|"; echo " .-~                                    _.;::/::::a::::::::/"; echo " :-._                               _.-~ ./::::::::d:::::::|"; echo " \`-._~-._                   _..__.-~ _.-~|::/::::::::::::::|"; echo "  /  ~-._~-._              / .__..--~----.YWWWWWWWWWWWWWWWP'"; echo " \_____(_;-._\.        _.-~_/       ~).. . \ "; echo "    /(_____  \`--...--~_.-~______..-+_______)"; echo "  .(_________/\`--...--~/    _/           /\ "; echo " /-._     \_     (___./_..-~__.....__..-~./"; echo " \`-._~-._   ~\--------~  .-~_..__.-~ _.-~"; echo "     ~-._~-._ ~---------'  / .__..--~"; echo "         ~-._\.        _.-~_/"; echo "             \`--...--~_.-~"; echo "              \`--...--~"; fi;
