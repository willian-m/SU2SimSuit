set terminal pdf enhanced color
set output "plots/Tmunu12.pdf"
plot "output/orbitAveraged12.out" u 1:5:6 w yerrorbars lines 7 ps .5 , "output/orbitAveraged21.out" u 1:5:6 w yerrorbars lines 6 ps .5
set output "plots/Tmunu13.pdf"
plot "output/orbitAveraged13.out" u 1:5:6 w yerrorbars lines 7 ps .5 , "output/orbitAveraged31.out" u 1:5:6 w yerrorbars lines 6 ps .5
set output "plots/Tmunu23.pdf"
plot "output/orbitAveraged23.out" u 1:5:6 w yerrorbars lines 7 ps .5 , "output/orbitAveraged32.out" u 1:5:6 w yerrorbars lines 6 ps .5
set output "plots/TmunuDiagonals.pdf"
plot "output/orbitAveraged11.out" u 1:5:6 w yerrorbars lines 7 ps .5 , "output/orbitAveraged22.out" u 1:5:6 w yerrorbars lines 6 ps .5, "output/orbitAveraged33.out" u 1:5:6 w yerrorbars lines 8 ps .5
