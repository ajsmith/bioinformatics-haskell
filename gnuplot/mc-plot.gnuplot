set term png
set output "metropolis.png"

set title "Simulation using Metropolis Monte Carlo Method"
set xlabel "Temperature"
set ylabel "<E>"
unset key

plot "metropolis-data.txt" using 2:3 with points pointtype 13
