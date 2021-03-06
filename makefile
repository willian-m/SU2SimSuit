BIN=bin
SRC=src
MODULES=$(SRC)/modules
OBJ_LAT_CONF= $(BIN)/ziggurat.o $(BIN)/mathSU2.o $(BIN)/lattice.o $(BIN)/physics.o $(BIN)/MonteCarlo.o $(BIN)/IOfunctions.o
OBJ_TENSOR= $(BIN)/ziggurat.o $(BIN)/mathSU2.o $(BIN)/lattice.o $(BIN)/physics.o $(BIN)/IOfunctions.o $(BIN)/Measurements.o
OBJ_FFT= $(MKLROOT)/include/mkl_dfti.o
FC=gfortran

MODE=R
#MODE can be R (Release, with optimazation flags), D (Debug) or P (Profiling)

ifeq ($(FC),ifort)

ifeq ($(MODE),D)
FFLAGS=-heap-arrays 4096
endif

ifeq ($(MODE),R)
FFLAGS=-heap-arrays 4096
endif

ifeq ($(MODE),P)
FFLAGS=-p -heap-arrays 4096
endif

MKL_LINK=-mkl
endif


ifeq ($(FC),gfortran)
MKL_LINK=-L${MKLROOT}/lib/intel64 -Wl,--no-as-needed -lmkl_rt -lpthread -lm -ldl
FFLAGS=-ffree-line-length-none
endif

all: gen_lat_conf.run tmunu_corr.run FFT_Tmunu.run stat_avrg_cmplx.run stat_avrg_dble.run orb_avrg_cmplx.run orb_avrg_dble.run 

orb_avrg_dble.run: dir $(SRC)/orb_avrg_dble.f90
	$(FC) $(FFLAGS) $(SRC)/orb_avrg_dble.f90 -o $(BIN)/$@

orb_avrg_cmplx.run: dir $(SRC)/orb_avrg_cmplx.f90
	$(FC) $(FFLAGS) $(SRC)/orb_avrg_cmplx.f90 -o $(BIN)/$@

stat_avrg_cmplx.run: dir $(SRC)/stat_avrg_cmplx.f90
	$(FC) $(FFLAGS) $(SRC)/stat_avrg_cmplx.f90 -o $(BIN)/$@

stat_avrg_dble.run: dir $(SRC)/stat_avrg_dble.f90
	$(FC) $(FFLAGS) $(SRC)/stat_avrg_dble.f90 -o $(BIN)/$@

FFT_Tmunu.run: dir $(SRC)/FFT_Tmunu.f90
	$(FC) -I${MKLROOT}/include  $(FFLAGS) $(SRC)/FFT_Tmunu.f90 $(MKL_LINK) -o $(BIN)/$@

gen_lat_conf.run: dir $(OBJ_LAT_CONF) $(SRC)/gen_lat_conf.f90
	$(FC) $(FFLAGS) -I$(BIN) $(OBJ_LAT_CONF) $(SRC)/gen_lat_conf.f90 -o $(BIN)/$@

tmunu_corr.run: dir $(OBJ_TENSOR) $(SRC)/tmunu_corr.f90
	$(FC) $(FFLAGS) -I$(BIN) $(OBJ_TENSOR) $(SRC)/tmunu_corr.f90 -o $(BIN)/$@

dir: 
	mkdir -p $(BIN)

$(BIN)/%.o: $(MODULES)/%.f90
	if [ $(FC) = ifort ]; then $(FC) $(FFLAGS) -c -module $(BIN) -o $@ $<; elif [ $(FC) = gfortran ]; then $(FC) $(FFLAGS) -c -J$(BIN) -o $@ $<; fi

clean:
	rm -f $(BIN)/*.o $(BIN)/*.mod $(BIN)/*.run
	rmdir $(BIN)

plots: plots/Tmunu12.pdf plots/Tmunu13.pdf plots/Tmunu23.pdf plots/TmunuDiagonals.pdf
	gnuplot plots/make_plots.plt

plots/Tmunu12.pdf: plots/make_plots.plt
	gnuplot plots/make_plots.plt

plots/Tmunu13.pdf: plots/make_plots.plt
	gnuplot plots/make_plots.plt

plots/Tmunu23.pdf: plots/make_plots.plt
	gnuplot plots/make_plots.plt

plots/TmunuDiagonals.pdf: plots/make_plots.plt
	gnuplot plots/make_plots.plt

sandwich:
	@USER="$(id -u)"
	@if [ $(USER) != "root" ]; then echo "Nice try, but one should not search for make commands in an internet comic strip."; fi;
	@if [ $(USER) = "root" ]; then echo "Ok. Here is your sandwich"; echo "                                                           _"; echo "                                                          //"; echo "                                                         //"; echo "                                         _______________//__"; echo "                                       .(______________//___)."; echo "                                       |              /      |"; echo "                                       |. . . . . . . / . . .|"; echo "                                       \ . . . . . ./. . . . /"; echo "                                        |           / ___   |"; echo "                   _.---._              |::......./../...\.:|"; echo "                _.-~       ~-._         |::::/::\::/:\::::::|"; echo "            _.-~               ~-._     |::::\::/::::::X:/::|"; echo "        _.-~                       ~---.;:::::::/::\::/:::::|"; echo "    _.-~                                 ~\::::::n::::::::::|"; echo " .-~                                    _.;::/::::a::::::::/"; echo " :-._                               _.-~ ./::::::::d:::::::|"; echo " \`-._~-._                   _..__.-~ _.-~|::/::::::::::::::|"; echo "  /  ~-._~-._              / .__..--~----.YWWWWWWWWWWWWWWWP'"; echo " \_____(_;-._\.        _.-~_/       ~).. . \ "; echo "    /(_____  \`--...--~_.-~______..-+_______)"; echo "  .(_________/\`--...--~/    _/           /\ "; echo " /-._     \_     (___./_..-~__.....__..-~./"; echo " \`-._~-._   ~\--------~  .-~_..__.-~ _.-~"; echo "     ~-._~-._ ~---------'  / .__..--~"; echo "         ~-._\.        _.-~_/"; echo "             \`--...--~_.-~"; echo "              \`--...--~"; fi;
