set terminal pdf enhanced color
set yrange [.07:.1]
set xlabel "p^2"
set output "plots/Tmunu12.pdf"
plot "output/orbitAveraged12.out" u ($1+$4**2):5:6 w yerrorbars lines 7 ps .3 t "<T_{01}(p^2) T_{02}(0)>", "output/orbitAveraged21.out" u ($1+$4**2):5:6 w yerrorbars lines 6 ps .3 t "<T_{02}(p^2) T_{01}(0)>"
set output "plots/Tmunu13.pdf"
plot "output/orbitAveraged13.out" u ($1+$4**2):5:6 w yerrorbars lines 7 ps .3 t "<T_{01}(p^2) T_{03}(0)>", "output/orbitAveraged31.out" u ($1+$4**2):5:6 w yerrorbars lines 6 ps .3 t "<T_{03}(p^2) T_{01}(0)>"
set output "plots/Tmunu23.pdf" 
plot "output/orbitAveraged23.out" u ($1+$4**2):5:6 w yerrorbars lines 7 ps .3 t "<T_{02}(p^2) T_{03}(0)>", "output/orbitAveraged32.out" u ($1+$4**2):5:6 w yerrorbars lines 6 ps .3 t "<T_{03}(p^2) T_{02}(0)>"
set output "plots/TmunuDiagonals.pdf" 
plot "output/orbitAveraged11.out" u ($1+$4**2):5:6 w yerrorbars lines 7 ps .3 t "<T_{01}(p^2) T_{01}(0)>", "output/orbitAveraged22.out" u ($1+$4**2):5:6 w yerrorbars lines 6 ps .3 t "<T_{02}(p^2) T_{02}(0)>", "output/orbitAveraged33.out" u ($1+$4**2):5:6 w yerrorbars lines 8 ps .3 t "<T_{03}(p^2) T_{03}(0)>"

